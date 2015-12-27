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
	Var4 Integer |				--może też var integer - jednoznaczne nazwy, ale trzeba też pamiętać w jakimś stanie, aby rozróżniać który który oraz w envie który aktualny
	Void4
    deriving (Eq,Ord,Show)	

type Env4 = Map.Map String Integer

type Vars4 = ReaderT Env4 (State (Integer,Integer))


--TODOTODO generowanie zmiennych temporalnych + labeli  - Stan - dla labeli tak, dla wyrażenia jak drzewo nie płaskie - wielokrotne rozgałęzienia? 
 
nextTemp :: Vars4 Integer
nextTemp = do
	(vars,temps) <- get
	put (vars,temps+1)
	return temps

newVar :: Vars4 Integer
newVar = do
	(vars,temps) <- get
	put (vars+1,temps)
	return vars





toCode4Expr :: Expr -> Vars4 ([Code4Instruction],ValVar4)

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
	
toCode4Expr (EVar (PIdent (_,x))) = do
	env <- ask 
	case Map.lookup x env of
		Just varnum -> return ([],Var4 varnum)
toCode4Expr (ELitInt n) = return ([],Int4 n)
toCode4Expr ELitTrue = return ([],Bool4 True)
toCode4Expr ELitFalse = return ([],Bool4 False)
toCode4Expr (EString str) = return ([],String4 str)


toCode4Ass :: Integer -> Code4Instruction -> Code4Instruction
toCode4Ass varnum (OpV op (Temp4 _) x1 x2) = (OpV op (Var4 varnum) x1 x2)
toCode4Ass varnum (Empty4 x) = Ass4 (Var4 varnum) x


toCode4Decl :: Type -> [Item] -> Vars4 (Env4,[Code4Instruction])
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



--TODO NInit - tutaj albo usunąć wcześniej - we frontendzie

toCode4Stmt :: Stmt -> Vars4 (Env4,[Code4Instruction])

toCode4Stmt Empty = do
	env <- ask
	return (env,[])

toCode4Stmt (BStmt (Block bl)) = do
	env <- ask
	r <- toCode4Block bl
	return (env,r)

toCode4Stmt (Decl t l) = toCode4Decl t l

toCode4Stmt (Ass (PIdent (_,varname)) exp) = do
	env <- ask
	(r,temp) <- toCode4Expr exp
	case Map.lookup varname env of
		Just varnum -> if (null r)
				then return (env,[toCode4Ass varnum (Empty4 temp)])
				else return (env,(init r) ++[toCode4Ass varnum (last r)])

--toCode4Stmt (Incr (PIdent (_,varname))) = return [] --TODO

--toCode4Stmt (Decr (PIdent (_,varname))) = return [] --TODO

toCode4Stmt (Ret _ exp) = do
	env <- ask
	(r,temp) <-toCode4Expr exp
	return  (env,r ++[Return4 temp])

toCode4Stmt (VRet _) = do
	env <- ask
	return (env,[Return4 Void4])

--TODOTODO conditionale








toCode4Stmt (SExp exp) = do		--TODO można zamienić OpV na OpE
	env <- ask
	(r,_) <- toCode4Expr exp
	return (env,r)



toCode4Block :: [Stmt] -> Vars4 [Code4Instruction]		--TODO inne
toCode4Block (stm:stmts) = do
	(env4,a1) <- toCode4Stmt stm
	a2 <- (local (\x -> env4) (toCode4Block stmts))
	return (a1++a2)
toCode4Block [] = return []


toCode4 :: TopDef -> Vars4 [Code4Instruction]			--TODO inne			--TODO powinno zapamiętać liczbę zmiennych lokalnych, żeby wiedzieć o ile przesunąć stos
toCode4 (FnDef _ _ _ (Block bl)) = toCode4Block bl


compileFunction :: TopDef -> StEnv ()
compileFunction (FnDef t (PIdent ((x,y),name)) args block) = return ()


compileFunctions ::[TopDef] ->StEnv ()

compileFunctions (f:fs) = do
	--compileFunction f
	let (code42,_) = runState (runReaderT (toCode4 f) (Map.empty)) (0,0)	--TODO zamiast empty argumenty
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
