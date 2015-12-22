module Latc_basic where

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

predefinedEnv :: Env
predefinedEnv = Map.fromList [("printInt",((0,0),0,0)),("printString",((0,0),1,0)),("error",((0,0),2,0)),("readInt",((0,0),3,0)),("readString",((0,0),4,0))] 
--TODO może jakiś inny wypis dla powtórzenia tych funkcji

predefinedSt :: (St,Integer)
predefinedSt = (Map.fromList [(0,(Fun Void [Int],Nothing)),(1,(Fun Void [Str],Nothing)),(2,(Fun Void [],Nothing)),(3,(Fun Int [],Nothing)),(4,(Fun Str [],Nothing))],5)

defaultVal :: Type -> Val
defaultVal Int = Just (Right 0)
defaultVal Bool = Just (Left (Right False))
defaultVal Str = Just (Left (Left ""))
