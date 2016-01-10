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
import Latc_printout

compileWhole :: Program -> IO ()
compileWhole (Program prog) = do
	(nprog,(st,_)) <- runStateT (runReaderT (checkProg prog) predefinedEnv) predefinedSt
	let code4 = toCode4 nprog
	let code42 = optimizeWhole code4
	assembleWhole code42
	hPutStrLn stderr ("OK\n")


lexerErrCheck :: Err Program -> IO()
lexerErrCheck (Ok e) = do
	let result = compileWhole e
	Control.Exception.catch (result) (\msg -> do
		hPutStrLn stderr $ "ERROR\n"++show(msg::Control.Exception.SomeException)
		exitWith (ExitFailure 1))
	exitWith ExitSuccess

lexerErrCheck (Bad s) = do
	hPutStrLn stderr ("ERROR\n" ++ s)
	exitWith (ExitFailure 1)

executeOnFile :: String -> IO()
executeOnFile s = lexerErrCheck (pProgram (myLexer s))

main = do
	args <- getArgs
	case args of
		[f] -> readFile f >>= executeOnFile
