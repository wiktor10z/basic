module Main(main) where

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
import Latc_basic
import Latc_ExpTypeVal

--TODO poprawny format wypisywania error

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
			let s = Map.insert loc (Fun t (argTypes args),Nothing) st 
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
				Just (Fun Int [],_) -> do return env
				Just (Fun _ (h:hs),_) -> error ("error main in line "++show(x)++" function cannot have arguments")
				Just (Fun _ _,_)	-> error ("error main function in line "++show(x)++" must return int")
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
			let s = Map.insert loc (t,Nothing) st 
			putSt s
			env2 <- (local (Map.insert name ((x,y),loc,1)) (checkArgs args))			
			return env2

		
checkArgs [] = ask
--	env <- ask
--	return env
	 
	 
insertVar :: Type -> Val -> ((Int,Int),String,Int) -> [Item]-> StEnv Env
insertVar t val (l,name,level) its = do
	st <- getSt	
	loc <- newLoc
	let s = Map.insert loc (t,val) st
	putSt s
	env2 <- (local (Map.insert name (l,loc,level)) (checkDecl t its level))
	return env2

duplicateVar :: String -> StEnv Env	--lokacja
duplicateVar name = do
	env <- ask
	st <- getSt
	case Map.lookup name env of
		Just ((x1,y1),loc,level)-> case (Map.lookup loc st) of
			Just (t,val) -> do			
				let s1 = Map.insert loc (t,Nothing) st
				loc2 <-newLoc	
				let s2 = Map.insert loc2 (t,val) s1
				putSt s2
				return (Map.insert name ((x1,y1),loc2,level) env)
			_ -> error "???"
		_ -> error "???"


	 
checkDecl :: Type -> [Item] -> Int -> StEnv Env

checkDecl t ((Init (PIdent ((x,y),name)) exp):its) level = do
	env <- ask
	st <- getSt
	(type2,val) <- checkExpTypeVal exp
	if type2==t
		then case (Map.lookup name env) of
			Just ((x1,y1),_,level2) -> if level==level2
				then error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
				else insertVar t val ((x,y),name,level) its
			_ -> insertVar t val ((x,y),name,level) its
		else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++name++" of different type")
			
checkDecl t ((NoInit (PIdent ((x,y),name))):its) level = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Just ((x1,y1),_,level2) -> if level==level2
										then error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
										else insertVar t (defaultVal t) ((x,y),name,level) its
		_ -> insertVar t (defaultVal t) ((x,y),name,level) its

checkDecl _ [] _ = ask

--TODO może odrazu obliczanie stałych, wtedy trzeba by dołożyć wartość do stanu



checkStmt :: Stmt ->Type -> Int -> StEnv Env			--wyrażenie -> typ funkcji -> poziom

checkStmt Empty _ _ = ask

checkStmt (BStmt (Block b)) ft level = do
	checkBlock b ft (level+1)
	ask
	
checkStmt (Decl t l) _ level = checkDecl t l level

checkStmt (Ass (PIdent ((x,y),name)) exp) _ _= do
	env <- ask
	st <- getSt
	(type2,val) <- checkExpTypeVal exp
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (t,_) -> if t==type2
				then do
				let s = Map.insert loc (t,val) st
				putSt s
				ask
				else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++name++" defined in line "++show(x1)++", column "++show(y1)++" of different type")
			--Nothing -> error "???"

checkStmt (Incr (PIdent ((x,y),name))) _ _= do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Int,Nothing) -> return env
			Just (Int,Just (Right n))-> do
				let s = Map.insert loc (Int,Just (Right (n+1))) st
				putSt s
				ask			
			_ -> error ("error in line "++show(x)++", column "++show(y)++" incrementation of variable "++name++" defined in line "++show(x1)++", column "++show(y1)++" of non-integer type")	

checkStmt (Decr (PIdent ((x,y),name))) _ _= do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Int,Nothing) -> return env
			Just (Int,Just (Right n))-> do
				let s = Map.insert loc (Int,Just (Right (n-1))) st
				putSt s
				ask			
			_ -> error ("error in line "++show(x)++", column "++show(y)++" decrementation of variable "++name++" defined in line "++show(x1)++", column "++show(y1)++" of non-integer type")

checkStmt (Ret (PReturn ((x,y),_)) exp) ft _ = case ft of
	Void -> error ("error in line "++show(x)++", column "++show(y)++"return with argument in procedure")
	_ -> do
		(type2,_) <- checkExpTypeVal exp
		if(type2==ft)
			then ask
			else error ("error in line "++show(x)++", column "++show(y)++" return with wrong type")

checkStmt (VRet (PReturn ((x,y),_))) ft _ = case ft of
	Void -> ask
	_ -> error ("error in line "++show(x)++", column "++show(y)++"return without argument in non-void function")

--TODOTODO tutaj 1


checkStmt (Cond (PIf ((x,y),_)) exp stm) ft level = do
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")

			_ -> error "abc"
		
		
			--case val of
			--	Just (Left (Right True)) -> 	--TODOTODO to się wykona zawsze - można usunąć if warunek itp. jak blok to ok, jak deklaracja to się wywal
			--	Just (Left (Right False)) ->	--TODOTODO to się i tak nie wykona, ale warto sprawdzić otypowanie - można by zrobić pierwszą optymalizację 
			--	Nothing ->						--TODOTODO to się może wykonać - jak deklaracja to się wywal, jak przypisanie, to daj wartość na nieokreśloną, jak blok to dalej ... z nieokreślonością, jak if czy coś to ???
		_ -> error ("error in line "++show(x)++", column "++show(y)++"if condition of non-boolean type")
		






















checkStmt (SExp exp) _ _ = do
	checkExpTypeVal exp
	ask


checkStmt _ _ _ = ask --TODO usunąć



checkBlock :: [Stmt] -> Type -> Int -> StEnv ()
checkBlock (stm:stmts) ft level = do
	env <- checkStmt stm ft level
	(local (\x -> env) (checkBlock stmts ft level))
	return ()
	
checkBlock  [] _ _ = return ()









checkFunction :: TopDef -> StEnv ()	-- po pierwsze wpisać zmienne do stenv, ale tylko lokalnie - zaczynając od tych z args, sprawdzać, czy deklaracje się nie dublują
checkFunction (FnDef t (PIdent ((x,y),name)) args (Block b)) = do
	env <- checkArgs args
	(local (\x -> env) (checkBlock b t 2))
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
