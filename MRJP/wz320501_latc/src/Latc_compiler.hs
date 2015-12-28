module Main(main) where

import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map

import Latte.Abs
import Latte.Lex
import Latte.Par
import Latte.ErrM
import Latc_basic
import Latc_frontend

type Code4Block = (String,[Code4Instruction])

data Code4Instruction =
	Empty4 ValVar4 |
	Ass4 ValVar4 ValVar4|
	OpE Op ValVar4 ValVar4 |
	OpV Op ValVar4 ValVar4 ValVar4 |
	Neg4 ValVar4 ValVar4 |
	Param4 ValVar4 |
	CallE String Integer |	
	CallV ValVar4 String Int |
	Return4 ValVar4	|
	Goto4 String |
	If4 ValVar4 String
    deriving (Eq,Ord,Show)
  
data Op =
	Add4 |
	Sub4 |
	Mul4 |
	Div4 |
	Mod4 |
	SetL4 |
	SetG4 |
	SetLE4 |
	SetGE4 |
	SetE4 |
	SetNE4 |
	And4 |
	Or4 |
	Xor4
    deriving (Eq,Ord,Show)



data ValVar4 =
	Int4 Integer |
	Bool4 Bool |
	String4 String |
	Temp4 Integer |
	Var4 Integer |				--może też var integer - jednoznaczne nazwy, ale trzeba też pamiętać w jakimś stanie, aby rozróżniać który który oraz w envie który aktualny
	Void4
    deriving (Eq,Ord,Show)	

type Env4 = Map.Map String Integer
type St4 = (String,Integer,String,Integer,Integer,[Code4Block],[Code4Instruction])	--nazwa, labele, zmienne, tempy

type StEnv4 = ReaderT Env4 (State St4)


--TODOTODO generowanie zmiennych temporalnych + labeli  - Stan - dla labeli tak, dla wyrażenia jak drzewo nie płaskie - wielokrotne rozgałęzienia? 
 
nextTemp :: StEnv4 Integer
nextTemp = do
	(name,labels,nextlabel,vars,temps,blocks,instrs) <- get
	put (name,labels,nextlabel,vars,temps+1,blocks,instrs)
	return temps

newVar :: StEnv4 Integer
newVar = do
	(name,labels,nextlabel,vars,temps,blocks,instrs) <- get
	put (name,labels,nextlabel,vars+1,temps,blocks,instrs)
	return vars

addInstructions :: [Code4Instruction] -> StEnv4 ()
addInstructions instrs2 = do
	(name,labels,nextlabel,vars,temps,blocks,instrs1) <- get
	put (name,labels,nextlabel,vars,temps,blocks,instrs1++instrs2)

writeBlock :: StEnv4 [Code4Block]
writeBlock = do
	(name,labels,nextlabel,vars,temps,blocks,instrs) <- get
	put (name,labels,nextlabel,vars,temps,blocks ++ [(nextlabel,instrs)],[])
	return (blocks ++ [(nextlabel,instrs)])

getLabel :: StEnv4 String
getLabel = do
	(name,labels,nextlabel,vars,temps,blocks,instrs) <- get
	put (name,labels+1,nextlabel,vars,temps,blocks,instrs)
	return (name ++ (show labels))

setLabel :: String -> StEnv4 ()
setLabel labelname = do
	(name,labels,_,vars,temps,blocks,instrs) <- get
	put (name,labels,labelname,vars,temps,blocks,instrs)


----------------------------------------------------------------------------------------------------------------------------------------------

toCode4App :: [Expr] -> StEnv4 [Code4Instruction]
toCode4App (exp:exps) = do
	(str,x) <- toCode4Expr exp
	str2 <- toCode4App exps
	return (str++[Param4 x]++str2)

toCode4App [] = return []


toCode4Expr :: Expr -> StEnv4 ([Code4Instruction],ValVar4)

toCode4Expr (EVar (PIdent (_,x))) = do
	env <- ask 
	case Map.lookup x env of
		Just varnum -> return ([],Var4 varnum)
toCode4Expr (ELitInt n) = return ([],Int4 n)
toCode4Expr ELitTrue = return ([],Bool4 True)
toCode4Expr ELitFalse = return ([],Bool4 False)

toCode4Expr (EApp (PIdent (_,name)) exps) = do
	str <- toCode4App exps
	temp <- nextTemp
	return (str++[CallV (Temp4 temp) name (length exps)],(Temp4 temp))

toCode4Expr (EString str) = return ([],String4 str)


toCode4Expr (Neg (PMinus _) (ELitInt n)) = return([],Int4 (-n))

toCode4Expr (Neg (PMinus _) exp) = do
	(str,x) <- toCode4Expr exp
	temp <- nextTemp
	return (str++[Neg4 (Temp4 temp) x],(Temp4 temp))

toCode4Expr (Not (PNot _) exp) = do
	(str,x) <- toCode4Expr exp
	temp <- nextTemp
	return (str++[OpV Xor4 (Temp4 temp) x (Bool4 True)],(Temp4 temp))

toCode4Expr (EMul exp1 (Times (PTimes _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Mul4 (Temp4 temp) x1 x2],(Temp4 temp))
	
toCode4Expr (EMul exp1 (Div (PDiv _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Div4 (Temp4 temp) x1 x2],(Temp4 temp))
	
toCode4Expr (EMul exp1 (Mod (PMod _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Mod4 (Temp4 temp) x1 x2],(Temp4 temp))
	
toCode4Expr (EAdd exp1 (Plus (PPlus _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Add4 (Temp4 temp) x1 x2],(Temp4 temp))

toCode4Expr (EAdd exp1 (Minus (PMinus _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Sub4 (Temp4 temp) x1 x2],(Temp4 temp))
	
toCode4Expr (ERel exp1 (LTH (PLTH _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV SetL4 (Temp4 temp) x1 x2],(Temp4 temp))	
	
toCode4Expr (ERel exp1 (LE (PLE _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV SetLE4 (Temp4 temp) x1 x2],(Temp4 temp))	
	
toCode4Expr (ERel exp1 (GTH (PGTH _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV SetG4 (Temp4 temp) x1 x2],(Temp4 temp))	
	
toCode4Expr (ERel exp1 (GE (PGE _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV SetGE4 (Temp4 temp) x1 x2],(Temp4 temp))
	
toCode4Expr (ERel exp1 (EQU (PEQU _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV SetE4 (Temp4 temp) x1 x2],(Temp4 temp))	
	
toCode4Expr (ERel exp1 (NE (PNE _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV SetNE4 (Temp4 temp) x1 x2],(Temp4 temp))

toCode4Expr (EAnd exp1 (PAnd _) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV And4 (Temp4 temp) x1 x2],(Temp4 temp))

toCode4Expr (EOr exp1 (POr _) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Or4 (Temp4 temp) x1 x2],(Temp4 temp))


toCode4Ass :: Integer -> Code4Instruction -> Code4Instruction
toCode4Ass varnum (OpV op (Temp4 _) x1 x2) = (OpV op (Var4 varnum) x1 x2)
toCode4Ass varnum (Empty4 x) = Ass4 (Var4 varnum) x


toCode4Decl :: Type -> [Item] -> StEnv4 (Env4,[Code4Instruction])
toCode4Decl t ((Init (PIdent (_,varname)) exp):its) = do
	(r,temp) <- toCode4Expr exp
	varnum <- newVar
	(env4,r2) <- (local (Map.insert varname varnum)  (toCode4Decl t its))
	if (null r)
		then return (env4,[toCode4Ass varnum (Empty4 temp)]++r2)
		else return (env4,(init r) ++[toCode4Ass varnum (last r)]++r2)

toCode4Decl _ [] = do
	env <- ask
	return (env,[])

toCode4Stmt :: Stmt -> StEnv4 Env4

toCode4Stmt Empty = ask

toCode4Stmt (BStmt (Block bl)) = do
	toCode4Block bl
	ask

toCode4Stmt (Decl t l) = do
	(env4,inst4) <- toCode4Decl t l
	addInstructions inst4
	return env4

toCode4Stmt (Ass (PIdent (_,varname)) exp) = do
	env <- ask
	(inst4,temp) <- toCode4Expr exp
	case Map.lookup varname env of
		Just varnum -> if (null inst4)
				then do
					addInstructions [toCode4Ass varnum (Empty4 temp)]
					ask
				else do
					addInstructions ((init inst4) ++[toCode4Ass varnum (last inst4)])
					ask

toCode4Stmt (Incr (PIdent (_,varname))) = do
	env <- ask
	case Map.lookup varname env of
		Just varnum -> do
			addInstructions [OpV Add4 (Var4 varnum) (Var4 varnum) (Int4 1)]
			ask

toCode4Stmt (Decr (PIdent (_,varname))) = do
	env <- ask
	case Map.lookup varname env of
		Just varnum -> do
			addInstructions [OpV Sub4 (Var4 varnum) (Var4 varnum) (Int4 1)]
			ask

toCode4Stmt (Ret _ exp) = do	--za tym nie ma co wrzucać, bo i tak koniec
	(inst4,temp) <-toCode4Expr exp
	addInstructions (inst4 ++[Return4 temp])
	ask

toCode4Stmt (VRet _) = do
	addInstructions [Return4 Void4]
	ask

toCode4Stmt (Cond (PIf _) exp stm) = do
	condlabel <- getLabel
	addInstructions [Goto4 condlabel]
	writeBlock
	(inst4,temp4) <- toCode4Expr exp	-- warunek typu bool - może być zmienna lub temp
	truelabel <- getLabel
	endlabel <- getLabel	
	setLabel truelabel
	toCode4Stmt stm	
	addInstructions [Goto4 endlabel]
	writeBlock		
	setLabel condlabel		
	addInstructions inst4
	addInstructions [If4 temp4 truelabel,Goto4 endlabel]
	writeBlock	
	setLabel endlabel
	ask
	--TODO jak był return to nie ma potrzeby dawać po nim Goto


toCode4Stmt (CondElse (PIf _) exp stm stm2) = do
	condlabel <- getLabel
	addInstructions [Goto4 condlabel]
	writeBlock
	(inst4,temp4) <- toCode4Expr exp
	truelabel <- getLabel
	falselabel <- getLabel
	endlabel <- getLabel
	setLabel truelabel
	toCode4Stmt stm	
	addInstructions [Goto4 endlabel]
	writeBlock
	setLabel falselabel
	toCode4Stmt stm2
	addInstructions [Goto4 endlabel]
	writeBlock	
	setLabel condlabel		
	addInstructions inst4
	addInstructions [If4 temp4 truelabel,Goto4 falselabel]
	writeBlock	
	setLabel endlabel
	ask

toCode4Stmt (While (PWhile _) exp stm) = do
	condlabel <- getLabel
	addInstructions [Goto4 condlabel]
	writeBlock
	(inst4,temp4) <- toCode4Expr exp
	truelabel <- getLabel
	endlabel <- getLabel
	setLabel truelabel
	toCode4Stmt stm	
	addInstructions [Goto4 condlabel]
	writeBlock		
	setLabel condlabel		
	addInstructions inst4
	addInstructions [If4 temp4 truelabel,Goto4 endlabel]
	writeBlock	
	setLabel endlabel
	ask

toCode4Stmt (SExp exp) = do		--TODO można zamienić OpV na OpE
	(inst4,_) <- toCode4Expr exp
	addInstructions inst4
	ask



toCode4Block :: [Stmt] -> StEnv4 ()		--TODO inne
toCode4Block (stm:stmts) = do
	env4 <- toCode4Stmt stm
	(local (\x -> env4) (toCode4Block stmts))
	
toCode4Block [] = return ()


toCode4 :: TopDef -> StEnv4 [Code4Block]		--TODO inne, topdefy, może potrzebne spisanie instrukcji do ostatniego bloku - można by w return, ale dla void może nie być return 
toCode4 (FnDef _ (PIdent (_,name)) _ (Block bl)) = do
	put (name,1,name++"0",0,0,[],[])
	toCode4Block bl
	(_,_,_,_,_,bl4,inst4) <- get
	if (null inst4)
		then return bl4
		else do
			writeBlock	--TODO generator numerów labeli - nazwa funkcji w stanie?
--TODO tutaj wstawić końcówkę do ostatniego bloku (jeśli niepusta)


compileFunction :: TopDef -> StEnv ()
compileFunction (FnDef t (PIdent ((x,y),name)) args block) = return ()


compileFunctions ::[TopDef] ->StEnv ()

compileFunctions (f:fs) = do
	--compileFunction f
	let (code42,_) = runState (runReaderT (toCode4 f) (Map.empty)) ("",0,"",0,0,[],[])	--TODO zamiast empty argumenty
	error (show code42)
	return ()
	
compileFunctions [] = return ()

compileProgram :: Program -> StEnv ()
compileProgram (Program p) = do
	env <- checkFunctionSignatures p
	np <- (local (\x ->env) (checkRest p))
	--error ((show p) ++"\n\n" ++(show np))
	--toCode4 np
	compileFunctions p				--TODO to ma chodzić na nowym np - p w celach debugowania
	return ()

compileWhole :: Program -> IO ()
compileWhole prog = do
	(_,(st,_)) <- runStateT (runReaderT (compileProgram prog) predefinedEnv) predefinedSt
	hPutStrLn stderr ("OK\n")
	return ()


lexerErrCheck :: Err Program -> IO()
lexerErrCheck (Ok e) = do
	Control.Exception.catch (compileWhole e) (\msg -> hPutStrLn stderr $ "ERROR\n"++show(msg::Control.Exception.SomeException))
	exitWith (ExitFailure 1)

lexerErrCheck (Bad s) = do
	hPutStrLn stderr ("ERROR\n" ++ s)
	exitWith (ExitFailure 1)

executeOnFile :: String -> IO()
executeOnFile s = lexerErrCheck (pProgram (myLexer s))

main = do
	args <- getArgs
	case args of
		[f] -> readFile f >>= executeOnFile
