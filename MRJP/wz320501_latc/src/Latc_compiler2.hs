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
	Empty4 |
	OpE Op ValVar ValVar |
	OpV Op String ValVar ValVar |
	CallE String [ValVar] |
	CallV String String [ValVar]
    deriving (Eq,Ord,Show)
  
data Op =
	Add4 |
	Sub4 |
	Mul4 |
	Div4 |
	Mod4 
    deriving (Eq,Ord,Show)


type ValVar = Either String Value4


data Value4 =
	Int4 Integer |
	Bool4 Bool |
	String4 String
    deriving (Eq,Ord,Show)	

type Temps4 = State Integer

--TODOTODOTODOTODOTODOTODO generowanie zmiennych temporalnych + labeli  - Stan - dla labeli tak, dla wyrażenia jak drzewo nie płaskie - wielokrotne rozgałęzienia?  

toCode4 :: Stmt -> Code4Instruction
toCode4 (SExp exp) =
	let (r1,_) toCode4Expr exp
	in r1

toCode4Expr :: Expr -> Temps4 (Code4Instruction,ValVar)

toCode4Expr (EAdd exp1 (Plus (PPlus _)) exp2) =
	let (_,x1) = toCode4Expr exp1
	in let (_,x2) = toCode4Expr exp2	
	in (OpE Add4 x1 x2,(Left ""))

toCode4Expr (EAdd exp1 (Minus (PMinus _)) exp2) =
	let (_,x1) = toCode4Expr exp1
	in let (_,x2) = toCode4Expr exp2	
	in (OpE Sub4 x1 x2,Left "")

toCode4Expr (EMul exp1 (Times (PTimes _)) exp2) =
	let (_,x1) = toCode4Expr exp1
	in let (_,x2) = toCode4Expr exp2	
	in (OpE Mul4 x1 x2,Left "")

toCode4Expr (EMul exp1 (Div (PDiv _)) exp2) =
	let (_,x1) = toCode4Expr exp1
	in let (_,x2) = toCode4Expr exp2	
	in (OpE Div4 x1 x2,Left "")
	
toCode4Expr (EMul exp1 (Mod (PMod _)) exp2) =
	let (_,x1) = toCode4Expr exp1
	in let (_,x2) = toCode4Expr exp2	
	in (OpE Div4 x1 x2,Left "")

toCode4Expr (EVar (PIdent (_,x))) = (Empty4,Left ("var_"++x))
toCode4Expr (ELitInt n) = (Empty4,Right (Int4 n))
toCode4Expr ELitTrue = (Empty4,Right (Bool4 True))
toCode4Expr ELitFalse = (Empty4,Right (Bool4 False))
toCode4Expr (EString str) = (Empty4,Right (String4 str))

compileFunction :: TopDef -> StEnv ()
compileFunction (FnDef t (PIdent ((x,y),name)) args block) = return ()


compileFunctions ::[TopDef] ->StEnv ()

compileFunctions (f:fs) = do
	compileFunction f
	compileFunctions fs
	return ()
	
compileFunctions [] = return ()

compileProgram :: Program -> StEnv ()
compileProgram (Program p) = do
	env <- checkFunctionSignatures p
	np <- (local (\x ->env) (checkRest p))
	--error ((show p) ++"\n\n" ++(show np))
	compileFunctions p
	return ()

compileWhole :: Program -> IO ()
compileWhole prog = do
	(_,(st,_)) <- runStateT (runReaderT (compileProgram prog) predefinedEnv) predefinedSt
	let code41 = toCode4 (SExp (EAdd (ELitInt 5) (Minus (PMinus ((0,0),"-"))) (EVar (PIdent ((0,0),"y")))))
	hPutStrLn stderr ("OK\n"++(show code41))
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
