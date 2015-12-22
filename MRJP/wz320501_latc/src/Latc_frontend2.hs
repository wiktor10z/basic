import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map

import Latte.Abs
import Latte.Lex
import Latte.Par
import Latte.ErrM

--TODO poprawny format wypisywania error

type Env = Map.Map String ((Int,Int),Integer,Int)			--(pozycja w kodzie),lokacja,poziom
type St = Map.Map Integer Type
type StEnv x = ReaderT Env (StateT (St, Integer) IO) x  

getSt :: StEnv St
getSt = do
	(s,_) <- get
	return s

putSt :: St -> StEnv ()
putSt st = do
	(_,loc) <- get
	put (st,loc)

newLoc :: StEnv Integer
newLoc = do
	(st,loc) <- get
	put(st, loc+1)
	return loc

predefinedEnv :: Env
predefinedEnv = Map.fromList [("printInt",((0,0),0,0)),("printString",((0,0),1,0)),("error",((0,0),2,0)),("readInt",((0,0),3,0)),("readString",((0,0),4,0))] 
--TODO może jakiś inny wypis dla powtórzenia tych funkcji

predefinedSt :: (St,Integer)
predefinedSt = (Map.fromList [(0,Fun Void [Int]),(1,Fun Void [Str]),(2,Fun Void []),(3,Fun Int []),(4,Fun Str [])],5)









argTypes :: [Arg] -> [Type]
argTypes ((Arg t pi):args) = t : (argTypes args)
argTypes [] = []

--TODO być może od razu sprawdzić czy nie ma argumentów o tych samych nazwach, albo można przy 2 przebiegu i sprawdzaniu unikalności deklaracji
checkFunctionSignatures :: [TopDef] -> StEnv Env
checkFunctionSignatures ((FnDef t (PIdent ((x,y),name)) args block):fs) = do
	env <- ask														
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> do
			loc <- newLoc
			let s = Map.insert loc (Fun t (argTypes args)) st 
			putSt s	
			env2 <- (local (Map.insert name ((x,y),loc,0)) (checkFunctionSignatures fs))			
			return env2
		Just ((x1,y1),_,_) -> error ("error in line "++show(x)++", column "++show(y)++" function "++name++" previously defined in line "++show(x1)++", column "++show(y1))

checkFunctionSignatures [] = do
	env <- ask
	case (Map.lookup "main" env) of
		Nothing -> error ("error no main function")
		Just ((x,y),(loc),_) -> do
			st <- getSt
			case (Map.lookup loc st) of				--TODO upewnić się, że case pattern matchuje jak funkcja - nie zrobi złego pattern matchingu (np. wiele patternów się zmaczuje)
				Just (Fun Int []) -> do return env
				Just (Fun _ (h:hs)) -> error ("error main in line "++show(x)++" function cannot have arguments")
				Just (Fun _ _)	-> error ("error main function in line "++show(x)++" must return int")
				-- Just (_) -> error "???"
				--Nothing -> error "???"



checkArgs :: [Arg] -> StEnv Env

checkArgs ((Arg t (PIdent ((x,y),name))):args) = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Just ((x1,y1),_,1) -> error ("error in line "++show(x)++", column "++show(y)++" argument "++name++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
		_ -> do
			loc <- newLoc
			let s = Map.insert loc t st 
			putSt s
			env2 <- (local (Map.insert name ((x,y),loc,1)) (checkArgs args))			
			return env2

		
checkArgs [] = ask
--	env <- ask
--	return env
	 
	 
insertVar :: Type -> ((Int,Int),String,Int) -> [Item]-> StEnv Env
insertVar t (l,name,level) its = do
	st <- getSt	
	loc <- newLoc
	let s = Map.insert loc t st
	putSt s
	env2 <- (local (Map.insert name (l,loc,level)) (checkDecl t its level))
	return env2
	 
	 
checkDecl :: Type -> [Item] -> Int -> StEnv Env

checkDecl t ((Init (PIdent ((x,y),name)) exp):its) level = do
	env <- ask
	st <- getSt
	type2 <- checkExpType exp
	if type2==t
		then case (Map.lookup name env) of
			Just ((x1,y1),_,level2) -> if level==level2
				then error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
				else insertVar t ((x,y),name,level) its
			_ -> insertVar t ((x,y),name,level) its
		else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++name++" of different type")
			
checkDecl t ((NoInit (PIdent ((x,y),name))):its) level = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Just ((x1,y1),_,level2) -> if level==level2
										then error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
										else insertVar t ((x,y),name,level) its
		_ -> insertVar t ((x,y),name,level) its

checkDecl _ [] _ = ask


multipleTypesMatch :: PIdent -> (Int,Int) -> [Expr] -> [Type] -> Int -> StEnv ()

multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) (exp:exps) (t:ts) i = do
	multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) exps ts (i+1)
	t2 <- checkExpType exp
	if t2==t
		then return ()
		else error ("error in line "++show(x)++", column "++show(y)++" argument "++show(i)++" type does not match with function "++name++" declared in line "++show(x1)++", column "++show(y1)++" argument")


multipleTypesMatch _ _ [] [] _ = return ()
multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) [] _ _ = error ("error in line "++show(x)++", column "++show(y)++" not enough arguments to use function "++name++" declared in line "++show(x1)++", column "++show(y1))
multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) _ [] _ = error ("error in line "++show(x)++", column "++show(y)++" too many arguments to use function "++name++" declared in line "++show(x1)++", column "++show(y1))

--TODO może odrazu obliczanie stałych, wtedy trzeba by dołożyć wartość do stanu
checkExpType :: Expr -> StEnv Type

checkExpType (EVar (PIdent ((x,y),name))) = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Fun _ _) -> error ("error in line "++show(x)++", column "++show(y)++" attempt to use function "++name++" declared in line "++show(x1)++", column "++show(y1))	
			Just t -> return t
			--Nothing -> error "???"

checkExpType (ELitInt _) = return Int
checkExpType ELitTrue = return Bool
checkExpType ELitFalse = return Bool

checkExpType (EApp (PIdent ((x,y),name)) exps) = do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" function "++name++" does not exist")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Fun t tlist) -> do
				multipleTypesMatch (PIdent ((x,y),name)) (x1,y1) exps tlist 1
				return t
			Just _ -> error ("error in line "++show(x)++", column "++show(y)++"attempt to use variable declared in line "++show(x1)++", column "++show(y1)++" as function")

checkExpType (EString _) = return Str

checkExpType (Neg (PNeg ((x,y),_)) exp) = do
	type1 <- checkExpType exp
	if type1==Int
		then return Int
		else error ("error integer negation of non-integer expresion at line "++show(x)++", column "++show(y))

checkExpType (Not (PNot ((x,y),_)) exp) = do
	type1 <- checkExpType exp
	if type1==Bool
		then return Bool
		else error ("error boolean negation of non-boolean expresion at line "++show(x)++", column "++show(y))
		
checkExpType (EMul exp1 (Times (PTimes ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Int
		else error ("non-integer at times operator at line "++show(x)++", column "++show(y))
		
checkExpType (EMul exp1 (Div (PDiv ((x,y),_))) exp2) = do				--TODO jak obliczanie stałych, to dodać sprawdzanie dzielenia przez 0 - osobny wypis
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Int
		else error ("non-integer at divide operator at line "++show(x)++", column "++show(y))

checkExpType (EMul exp1 (Mod (PMod ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Int
		else error ("non-integer at modulo operator at line "++show(x)++", column "++show(y))		
checkExpType (EAdd exp1 (Plus (PPlus ((x,y),_))) exp2) = do		--TODO można wypisywać z której strony operatora jest błąd jak tylko z jednej
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if type1==type2
		then if ((type1==Int)||(type1==Str))
			then return type1
			else error ("wrong type at plus operator at line "++show(x)++", column "++show(y))
		else error ("not matching types at plus operator at line "++show(x)++", column "++show(y))

checkExpType (EAdd exp1 (Minus (PMinus ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Int
		else error ("non-integer at minus operator at line "++show(x)++", column "++show(y))

checkExpType (ERel exp1 (LTH (PLTH ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Bool
		else error ("non-integer at < operator at line "++show(x)++", column "++show(y))

checkExpType (ERel exp1 (LE (PLE ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Bool
		else error ("non-integer at <= operator at line "++show(x)++", column "++show(y))
		
checkExpType (ERel exp1 (GTH (PGTH ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Bool
		else error ("non-integer at > operator at line "++show(x)++", column "++show(y))

checkExpType (ERel exp1 (GE (PGE ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Int) && (type2==Int))
		then return Bool
		else error ("non-integer at >= operator at line "++show(x)++", column "++show(y))

checkExpType (ERel exp1 (EQU (PEQU ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if type1==type2
		then return Bool
		else error ("non-integer at >= operator at line "++show(x)++", column "++show(y))
		
checkExpType (ERel exp1 (NE (PNE ((x,y),_))) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if type1==type2
		then return Bool
		else error ("non-integer at >= operator at line "++show(x)++", column "++show(y))

checkExpType (EAnd exp1 (PAnd ((x,y),_)) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Bool) && (type2==Bool))
		then return Bool
		else error ("non-boolean at and operator at line "++show(x)++", column "++show(y))		

checkExpType (EOr exp1 (POr ((x,y),_)) exp2) = do
	type1 <- checkExpType exp1
	type2 <- checkExpType exp2
	if ((type1==Bool) && (type2==Bool))
		then return Bool
		else error ("non-boolean at or operator at line "++show(x)++", column "++show(y))	


--TODOTODO tutaj 1
checkStmt :: Stmt -> Int -> StEnv Env

checkStmt Empty _ = ask

checkStmt (BStmt (Block b)) level = do
	checkBlock b (level+1)
	ask
	
checkStmt (Decl t l) level = checkDecl t l level

checkStmt (Ass (PIdent ((x,y),name)) exp) _= do
	env <- ask
	st <- getSt
	type2 <- checkExpType exp
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just t -> if t==type2
				then ask
				else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++name++" defined in line "++show(x1)++", column "++show(y1)++" of different type")
			--Nothing -> error "???"

checkStmt (SExp exp) _ = do
	checkExpType exp
	ask

checkStmt _ _ = ask --TODO usunąć



checkBlock :: [Stmt] -> Int -> StEnv ()
checkBlock (stm:stmts) level = do
	env <- checkStmt stm level
	(local (\x -> env) (checkBlock stmts level))
	return ()
	
checkBlock  [] _ = return ()









checkFunction :: TopDef -> StEnv ()	-- po pierwsze wpisać zmienne do stenv, ale tylko lokalnie - zaczynając od tych z args, sprawdzać, czy deklaracje się nie dublują
checkFunction (FnDef t (PIdent ((x,y),name)) args (Block b)) = do
	env <- checkArgs args
	(local (\x -> env) (checkBlock b 2))
	--w zmienionym środowisku i stanie sprawdź resztę programu
	--lecąc instrukcja po instrukcji. Co jak blok wewnętrzny i deklaracja?
	--przy wejściu do bloku musimy zapamiętać środowisko, i w bloku zmieniać je i pamiętać, że deklaracje wewnątrz są inne niż na zewnątrz
	--trzeba albo przy wejściu zmienić w lokal środowisku aktualne zmienne wewnętrzne na zewnętrzne, albo do zmiennej pamiętać poziom
	return ()

checkRest :: [TopDef] -> StEnv ()
checkRest (f:fs) = do
	checkFunction f
	checkRest fs
	return ()

checkRest [] = return ()










compileFunction :: TopDef -> StEnv ()
compileFunction (FnDef t (PIdent ((x,y),name)) args block) = return ()
--tutaj zrobić sprawdzenie unikalności nazwy, zadeklarować nazwę z sygnaturą do pamięci
--typ wyjściowy itp.
--chyba kilkukrotny przebieg, ze względu na brak kolejności

compileFunctions ::[TopDef] ->StEnv ()

compileFunctions (f:fs) = do
	compileFunction f
	compileFunctions fs
	return ()
	
compileFunctions [] = return ()

compileProgram :: Program -> StEnv ()
compileProgram (Program p) = do
	env <- checkFunctionSignatures p
	(local (\x ->env) (checkRest p))
	--tutaj wiem już, że funkcje mają unikalne nazwy i main ma poprawny typ, nie wiem, czy zwracają co mają ani, czy nie powtarzają się identyfikatory w środku lub nawet wśród zmiennych wejścia
	--przydało by się usunąć if false, if true, while false . Co jak while true nie ma wyjścia
	compileFunctions p
	return ()

compileWhole :: Program -> IO ()
compileWhole prog = do
	(_,(st,_)) <- runStateT (runReaderT (compileProgram prog) predefinedEnv) predefinedSt
	hPutStrLn stderr (show st)
	return ()


lexerErrCheck :: Err Program -> IO()
lexerErrCheck (Ok e) = compileWhole e
	

lexerErrCheck (Bad s) = do
	hPutStrLn stderr "ERROR"
	hPutStrLn stderr s
	exitWith (ExitFailure 1)


executeOnFile :: String -> IO()
executeOnFile s = lexerErrCheck (pProgram (myLexer s))

main = do
	args <- getArgs
	case args of
		[f] -> readFile f >>= executeOnFile
