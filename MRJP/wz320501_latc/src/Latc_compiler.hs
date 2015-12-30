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

assembleBlocks :: [Code4Block] -> IO()
assembleBlocks b = return ()


assembleTopDef :: [Code4Block] -> IO()--tutaj wypisać prolog i epilog funkcji
assembleTopDef bs  = do
	assembleBlocks bs


assembleWhole :: [[Code4Block]] -> IO ()
assembleWhole (f:fs) = do
	assembleTopDef f
	assembleWhole fs

assembleWhole [] = return ()


compileWhole :: Program -> IO ()
compileWhole (Program prog) = do
	(nprog,(st,_)) <- runStateT (runReaderT (checkProg prog) predefinedEnv) predefinedSt
	let code4 = toCode4 prog				--TODO zamiast prog nprog - na razie w celach lepszego oglądania bez optymalizacji
	hPutStrLn stderr ("OK\n"++(show code4))
	assembleWhole code4
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
