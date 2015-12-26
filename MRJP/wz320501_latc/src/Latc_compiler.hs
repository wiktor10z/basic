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
import Latc_ExpTypeVal
import Latc_frontend

type BlockGraph = Map.Map Integer Code4Block



data Code4Block = Label [Code4Instruction] [Code4Block] [Code4Block]
data Code4Instruction =
	Empty4 ValVar4 |
	Ass4 ValVar4 ValVar4|
	OpE Op ValVar4 ValVar4 |
	OpV Op ValVar4 ValVar4 ValVar4 |
	CallE String [ValVar4] |
	CallV ValVar4 String [ValVar4] |
	Return4 ValVar4	
    deriving (Eq,Ord,Show)
  
data Op =
	Add4 |
	Sub4 |
	Mul4 |
	Div4 |
	Mod4 
    deriving (Eq,Ord,Show)



data ValVar4 =
	Int4 Integer |
	Bool4 Bool |
	String4 String |
	Temp4 Integer |
	Var4 String	|				--może też var integer - jednoznaczne nazwy, ale trzeba też pamiętać w jakimś stanie, aby rozróżniać który który oraz w envie który aktualny
	Void4
    deriving (Eq,Ord,Show)	

type Temps4 = State Integer


nextTemp :: Temps4 Integer
nextTemp = do
	temps <- get
	put (temps+1)
	return temps

type Vars4 = ReaderT (Map.Map String Integer) (State Integer)


--TODOTODO generowanie zmiennych temporalnych + labeli  - Stan - dla labeli tak, dla wyrażenia jak drzewo nie płaskie - wielokrotne rozgałęzienia?  


toCode4Expr :: Expr -> Temps4 ([Code4Instruction],ValVar4)

toCode4Expr (EAdd exp1 (Plus (PPlus _)) exp2) = do					--dla apply param x ... call f , n
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Add4 (Temp4 temp) x1 x2],(Temp4 temp))

toCode4Expr (EAdd exp1 (Minus (PMinus _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- nextTemp
	return (str1++str2++[OpV Sub4 (Temp4 temp) x1 x2],(Temp4 temp))
	--TODO reszta operatorów - można później - to tylko praca odtwórcza, te powinny wystarczyć do celów programowania
	
toCode4Expr (EVar (PIdent (_,x))) = return ([],Var4 x)
toCode4Expr (ELitInt n) = return ([],Int4 n)
toCode4Expr ELitTrue = return ([],Bool4 True)
toCode4Expr ELitFalse = return ([],Bool4 False)
toCode4Expr (EString str) = return ([],String4 str)


toCode4Ass :: String -> Code4Instruction -> Code4Instruction
toCode4Ass varname (OpV op (Temp4 _) x1 x2) = (OpV op (Var4 varname) x1 x2)
toCode4Ass varname (Empty4 x) = Ass4 (Var4 varname) x


toCode4Decl :: Type -> [Item] -> [Code4Instruction]		--trzeba by dodać jakieś zliczanie zmiennych lokalnych - dla deklaracji przesunięcia w funkcji (alokacji)
toCode4Decl t ((Init (PIdent (_,varname)) exp):its) =
	let ((r,temp),_) = runState (toCode4Expr exp) 0
	in if (null r)
		then [toCode4Ass varname (Empty4 temp)]
		else (init r) ++[toCode4Ass varname (last r)]


toCode4Stmt :: Stmt -> [Code4Instruction]

toCode4Stmt Empty = []

toCode4Stmt (BStmt (Block bl)) = toCode4Block bl

toCode4Stmt (Decl t l) = toCode4Decl t l

toCode4Stmt (Ass (PIdent (_,varname)) exp) =
	let ((r,temp),_) = runState (toCode4Expr exp) 0
	in if (null r)
		then [toCode4Ass varname (Empty4 temp)]
		else (init r) ++[toCode4Ass varname (last r)]

toCode4Stmt (Incr (PIdent (_,varname))) = [] --TODO

toCode4Stmt (Decr (PIdent (_,varname))) = [] --TODO

toCode4Stmt (Ret _ exp) =
	let ((r,temp),_) = runState (toCode4Expr exp) 0
	in (r ++[Return4 temp])

toCode4Stmt (VRet _) = [Return4 Void4]

--TODOTODO conditionale








toCode4Stmt (SExp exp) =		--TODO można zamienić OpV na OpE
	let ((r,_),_) = runState (toCode4Expr exp) 0
	in r



toCode4Block :: [Stmt] -> [Code4Instruction]		--TODO inne
toCode4Block (stm:stmts) = (toCode4Stmt stm)++(toCode4Block stmts)
toCode4Block [] = []


toCode4 :: TopDef -> [Code4Instruction]			--TODO inne			--TODO powinno zapamiętać liczbę zmiennych lokalnych, żeby wiedzieć o ile przesunąć stos
toCode4 (FnDef _ _ _ (Block bl)) = toCode4Block bl


compileFunction :: TopDef -> StEnv ()
compileFunction (FnDef t (PIdent ((x,y),name)) args block) = return ()


compileFunctions ::[TopDef] ->StEnv ()

compileFunctions (f:fs) = do
	--compileFunction f
	let code42 = toCode4 f	
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
