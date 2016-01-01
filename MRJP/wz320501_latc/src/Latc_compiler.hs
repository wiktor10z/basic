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
import Latc_Code4
import Latc_optimize

--TODO w jakimś środowisku i stanie trzeba pamietać co jest na którym rejestrze - i nie zwalniać ich za szybko - w code4 można zapamiętywać zmienne żywe (tempy- wysokość stosu))

assembleInstruction :: Code4Instruction -> IO()
assembleInstruction (Ass4 (Var4 var) (Int4 n)) = putStrLn ("    movq\t $"++(show n)++", -"++(show var)++"(%rbp)")

assembleInstruction (Return4 Void4) = putStrLn ("    leave\n    ret")
assembleInstruction (Return4 (Var4 var)) = putStrLn ("    movq\t"++(show ((-1)*var))++"(%rbp) %rax\n    leave\n    ret")

assembleInstruction _ = return ()


assembleInstrs :: [Code4Instruction] -> IO()
assembleInstrs (inst:instrs) = do
	assembleInstruction inst
	assembleInstrs instrs
	
assembleInstrs [] = return ()
	
assembleBlock :: Code4Block -> IO() --TODO to blok zerowy powinien zawierać push\q...
assembleBlock (label,instrs) = do
	putStrLn ("."++label++":")
	assembleInstrs instrs



assembleFunCode :: [Code4Block] -> IO()
assembleFunCode (b:bs) = do
	assembleBlock b
	assembleFunCode bs

assembleFunCode [] = return ()


assembleTopDef :: Code4Function -> IO()--tutaj wypisać prolog i epilog funkcji
assembleTopDef (name,((label,inst):bs),vars,temps)  = do
	putStrLn (name++":\n"++"."++(show label)++":\n    pushq\t %rbp\n    movq\t %rsp, %rbp\n    subq\t $"++(show vars)++", %rsp")
	assembleInstrs inst
	assembleFunCode bs


assembleWhole :: [Code4Function] -> IO ()
assembleWhole (f:fs) = do
	assembleTopDef f
	assembleWhole fs

assembleWhole [] = return ()


compileWhole :: Program -> IO ()
compileWhole (Program prog) = do
	hPutStrLn stderr ("OK\n"++(show prog))
	(nprog,(st,_)) <- runStateT (runReaderT (checkProg prog) predefinedEnv) predefinedSt
	hPutStrLn stderr ("OK\n"++(show nprog))
	let code4 = toCode4 prog				--TODO zamiast prog nprog - na razie w celach lepszego oglądania bez optymalizacji pierwotnej
	hPutStrLn stderr ("OK\n"++(show code4))
	let code42 = optimizeWhole code4
	hPutStrLn stderr ("OK\n"++(show code42))
	assembleWhole code42
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
