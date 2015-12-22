module Latc_ExpTypeVal where

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

multipleTypesMatch :: PIdent -> (Int,Int) -> [Expr] -> [Type] -> Int -> StEnv ()

multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) (exp:exps) (t:ts) i = do
	multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) exps ts (i+1)
	(t2,_) <- checkExpTypeVal exp
	if t2==t
		then return ()
		else error ("error in line "++show(x)++", column "++show(y)++" argument "++show(i)++" type does not match with function "++name++" declared in line "++show(x1)++", column "++show(y1)++" argument")


multipleTypesMatch _ _ [] [] _ = return ()
multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) [] _ _ = error ("error in line "++show(x)++", column "++show(y)++" not enough arguments to use function "++name++" declared in line "++show(x1)++", column "++show(y1))
multipleTypesMatch (PIdent ((x1,y1),name)) (x,y) _ [] _ = error ("error in line "++show(x)++", column "++show(y)++" too many arguments to use function "++name++" declared in line "++show(x1)++", column "++show(y1))





checkExpTypeVal :: Expr -> StEnv (Type,Val)

checkExpTypeVal (EVar (PIdent ((x,y),name))) = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Fun _ _,_) -> error ("error in line "++show(x)++", column "++show(y)++" attempt to use function "++name++" declared in line "++show(x1)++", column "++show(y1))	
			Just (t,val) -> return (t,val)
			--Nothing -> error "???"

checkExpTypeVal (ELitInt n) = return (Int,Just (Right n))
checkExpTypeVal ELitTrue = return (Bool,Just (Left (Right True)))
checkExpTypeVal ELitFalse = return (Bool,Just (Left (Right False)))

checkExpTypeVal (EApp (PIdent ((x,y),name)) exps) = do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" function "++name++" does not exist")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just ((Fun t tlist),_) -> do
				multipleTypesMatch (PIdent ((x,y),name)) (x1,y1) exps tlist 1
				return (t,Nothing)
			Just _ -> error ("error in line "++show(x)++", column "++show(y)++"attempt to use variable declared in line "++show(x1)++", column "++show(y1)++" as function")

checkExpTypeVal (EString str) = return (Str,Just (Left (Left str)))

checkExpTypeVal (Neg (PNeg ((x,y),_)) exp) = do
	(type1,val) <- checkExpTypeVal exp
	if type1==Int
		then case val of
			Nothing -> return (Int,Nothing)
			Just (Right n) -> return (Int,Just (Right (-n)))
			--Just _ -> error "???"
		else error ("error integer negation of non-integer expresion at line "++show(x)++", column "++show(y))

checkExpTypeVal (Not (PNot ((x,y),_)) exp) = do
	(type1,val) <- checkExpTypeVal exp
	if type1==Bool
		then case val of
			Nothing -> return (Bool,Nothing)
			Just (Left(Right b)) -> return (Bool,Just (Left (Right (not b))))
		else error ("error boolean negation of non-boolean expresion at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (EMul exp1 (Times (PTimes ((x,y),_))) exp2) = do		--TODO można wypisywać z której strony operatora jest błąd jak tylko z jednej
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Int,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Int,Nothing)
				Just (Right n2) -> return (Int,Just (Right (n1*n2)))
		else error ("error non-integer at times operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (EMul exp1 (Div (PDiv ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val2 of
			Nothing -> return (Int,Nothing)
			Just (Right 0) -> error ("error divide by 0 at line "++show(x)++", column "++show(y))
			Just (Right n2) -> case val1 of 
				Nothing -> return (Int,Nothing)
				Just (Right n1) -> return (Int,Just (Right (div n1 n2)))
		else error ("error non-integer at divide operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EMul exp1 (Mod (PMod ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val2 of
			Nothing -> return (Int,Nothing)
			Just (Right 0) -> error ("error modulo by 0 at line "++show(x)++", column "++show(y))
			Just (Right n2) -> case val1 of 
				Nothing -> return (Int,Nothing)
				Just (Right n1) -> return (Int,Just (Right (mod n1 n2)))
		else error ("non-integer at modulo operator at line "++show(x)++", column "++show(y))		
		
checkExpTypeVal (EAdd exp1 (Plus (PPlus ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if type1==type2
		then case type1 of
			Int -> case val1 of
				Nothing -> return (Int,Nothing)
				Just (Right n1) -> case val2 of 
					Nothing -> return (Int,Nothing)
					Just (Right n2) -> return (Int,Just (Right (n1+n2)))
			Str -> case val1 of
				Nothing -> return (Str,Nothing)
				Just (Left (Left s1)) -> case val2 of 
					Nothing -> return (Str,Nothing)
					Just (Left (Left s2)) -> return (Int,Just (Left (Left (s1++s2))))
			_ -> error ("wrong type at plus operator at line "++show(x)++", column "++show(y))
		else error ("not matching types at plus operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EAdd exp1 (Minus (PMinus ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Int,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Int,Nothing)
				Just (Right n2) -> return (Int,Just (Right (n1-n2)))
		else error ("non-integer at minus operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (LTH (PLTH ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing)
				Just (Right n2) -> return (Bool,Just (Left (Right (n1<n2))))
		else error ("non-integer at < operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (LE (PLE ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing)
				Just (Right n2) -> return (Bool,Just (Left (Right (n1<=n2))))
		else error ("non-integer at <= operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (ERel exp1 (GTH (PGTH ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing)
				Just (Right n2) -> return (Bool,Just (Left (Right (n1>n2))))
		else error ("non-integer at > operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (GE (PGE ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing)
				Just (Right n2) -> return (Bool,Just (Left (Right (n1>=n2))))
		else error ("non-integer at >= operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (EQU (PEQU ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if type1==type2
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just _ -> case val2 of
				Nothing -> return (Bool,Nothing)
				Just _ -> return (Bool,Just (Left (Right (val1==val2))))
		else error ("different types at == operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (ERel exp1 (NE (PNE ((x,y),_))) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if type1==type2
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just _ -> case val2 of
				Nothing -> return (Bool,Nothing)
				Just _ -> return (Bool,Just (Left (Right (val1/=val2))))
		else error ("different types at != operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EAnd exp1 (PAnd ((x,y),_)) exp2) = do		--TODO można spróbować leniwie, choć chyba nie ma sensu pomijać sprawdzania poprawności
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Bool) && (type2==Bool))
		then case val1 of
			Just (Left (Right False)) -> return (Bool,Just (Left (Right False)))
			Just (Left (Right True)) -> return (Bool,val2)			
			Nothing -> case val2 of
				Just (Left (Right False)) -> return (Bool,Just (Left (Right False)))
				_ -> return (Bool,Nothing)
		else error ("non-boolean at and operator at line "++show(x)++", column "++show(y))		

checkExpTypeVal (EOr exp1 (POr ((x,y),_)) exp2) = do
	(type1,val1) <- checkExpTypeVal exp1
	(type2,val2) <- checkExpTypeVal exp2
	if ((type1==Bool) && (type2==Bool))
		then case val1 of
			Just (Left (Right True)) -> return (Bool,Just (Left (Right True)))
			Just (Left (Right False)) -> return (Bool,val2)			
			Nothing -> case val2 of
				Just (Left (Right True)) -> return (Bool,Just (Left (Right True)))
				_ -> return (Bool,Nothing)
		else error ("non-boolean at or operator at line "++show(x)++", column "++show(y))	
