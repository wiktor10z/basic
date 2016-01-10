module Latc_basic where

import Control.Monad.Reader
import Control.Monad.State
import System.Environment
import qualified Data.Map as Map

import Latte.Abs


--stan i środowisko używane we frontendzie - zawierają zmienne z typami, ich wartości jeżeli można je łatwo ustalić oraz funkcje z typowaniem
type Val = Maybe (Either (Either String Bool) Integer)

type Env = Map.Map String ((Int,Int),Integer,Int)
type St = Map.Map Integer (Type,Val)
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

predefinedNames = ["printInt","printString","error","readInt","readString"]

predefinedEnv :: Env
predefinedEnv = Map.fromList [("printInt",((0,0),0,0)),("printString",((0,0),1,0)),("error",((0,0),2,0)),("readInt",((0,0),3,0)),("readString",((0,0),4,0))] 

predefinedSt :: (St,Integer)
predefinedSt = (Map.fromList [(0,(Fun Void [Int],Nothing)),(1,(Fun Void [Str],Nothing)),(2,(Fun Void [],Nothing)),(3,(Fun Int [],Nothing)),(4,(Fun Str [],Nothing))],5)

defaultVal :: Type -> Val
defaultVal Int = Just (Right 0)
defaultVal Bool = Just (Left (Right False))
defaultVal Str = Just (Left (Left ""))

defaultValExpr :: Type -> Expr
defaultValExpr Int = ELitInt 0
defaultValExpr Bool = ELitFalse
defaultValExpr Str = EString ""

valSize :: Type -> Int
valSize Int = 4
valSize Bool = 1
valSize Str = 8

argTypes :: [Arg] -> [Type]
argTypes ((Arg t pi):args) = t : (argTypes args)
argTypes [] = []

--zmiana typów w liczby - dla przekazywanie w łatwiejszy sposób pomiędzy częściami kompilatora

varType :: Type -> Int
varType Void = 0
varType Bool = 1
varType Int = 2
varType Str = 3

fromVarType :: Int -> Type
fromVarType 0 = Void
fromVarType 1 = Bool
fromVarType 2 = Int
fromVarType 3 = Str
