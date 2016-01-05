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


multipleTypesMatch :: PIdent -> (Int,Int) -> [Expr] -> [Type] -> Int -> StEnv [Expr]				--TODO zwróć uproszczone exps

multipleTypesMatch (PIdent ((x,y),name)) (x1,y1) (exp:exps) (t:ts) i = do
	(t2,_,nexp) <- checkExpTypeVal exp
	if t2==t
		then do
			nexps <- multipleTypesMatch (PIdent ((x,y),name)) (x1,y1) exps ts (i+1)
			return (nexp:nexps)
		else error ("error in line "++show(x)++", column "++show(y)++" argument "++show(i)++" type does not match with function "++show(name)++" declared in line "++show(x1)++", column "++show(y1)++" argument")


multipleTypesMatch _ _ [] [] _ = return []

checkExpTypeVal :: Expr -> StEnv (Type,Val,Expr)

checkExpTypeVal (EVar (PIdent ((x,y),name))) = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Fun _ _,_) -> error ("error in line "++show(x)++", column "++show(y)++" attempt to use function "++show(name)++" declared in line "++show(x1)++", column "++show(y1))	
			Just (t,val) -> case val of
				Nothing -> return (t,val,(EVar (PIdent ((x,y),name))))
				Just (Right n) -> if n>=0 
					then return (t,val,ELitInt n)
					else return (t,val,Neg (PMinus ((0,0),"-")) (ELitInt (-n)))
				Just (Left (Right True)) -> return (t,val,ELitTrue)
				Just (Left (Right False)) -> return (t,val,ELitFalse)
				Just (Left (Left str)) -> return (t,val,(EString str))
			--Nothing -> error "???"

checkExpTypeVal (ELitInt n) = return (Int,Just (Right n),ELitInt n)
checkExpTypeVal ELitTrue = return (Bool,Just (Left (Right True)),ELitTrue)
checkExpTypeVal ELitFalse = return (Bool,Just (Left (Right False)),ELitFalse)

checkExpTypeVal (EApp (PIdent ((x,y),name)) exps) = do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" function "++show(name)++" does not exist")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just ((Fun t tlist),_) -> 
				if((length exps) /= (length tlist))
					then error ("error in line "++show(x)++", column "++show(y)++" function "++show(name)++" declared in line "++show(x1)++", column "++show(y1)++" require "++(show (length tlist))++" arguments but is given "++(show (length exps)))
				else do
					nexps <- multipleTypesMatch (PIdent ((x,y),name)) (x1,y1) exps tlist 1
					return (t,Nothing,(EApp (PIdent ((varType t,0),name)) nexps))
			Just _ -> error ("error in line "++show(x)++", column "++show(y)++"attempt to use variable declared in line "++show(x1)++", column "++show(y1)++" as function")

checkExpTypeVal (EString str) = return (Str,Just (Left (Left str)),(EString str))

checkExpTypeVal (Neg (PMinus ((x,y),_)) exp) = do
	(type1,val,nexp) <- checkExpTypeVal exp
	if type1==Int
		then case val of
			Nothing -> return (Int,Nothing,(Neg (PMinus ((x,y),"-")) nexp))
			Just (Right n) -> if (-n)>=0 
					then return (Int,Just (Right (-n)),ELitInt n)
					else return (Int,Just (Right (-n)),Neg (PMinus ((x,y),"-")) (ELitInt (-n)))
			--Just _ -> error "???"
		else error ("integer negation of non-integer expresion at line "++show(x)++", column "++show(y))

checkExpTypeVal (Not (PNot ((x,y),_)) exp) = do
	(type1,val,nexp) <- checkExpTypeVal exp
	if type1==Bool
		then case val of
			Nothing -> return (Bool,Nothing,(Not (PNot ((x,y),"!")) nexp))
			Just (Left(Right True)) -> return (Bool,Just(Left (Right False)),ELitFalse)
			Just (Left(Right False)) -> return (Bool,Just(Left (Right True)),ELitTrue)
		else error ("boolean negation of non-boolean expresion at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (EMul exp1 (Times (PTimes ((x,y),_))) exp2) = do				--razy 0 można dać jako osobny przypadek -> zawsze 0, podobnie razy 1 to samo
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Int,Nothing,(EMul nexp1 (Times (PTimes ((x,y),"*"))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Int,Nothing,(EMul nexp1 (Times (PTimes ((x,y),"*"))) nexp2))
				Just (Right n2) -> if n1*n2>=0
					then return (Int,Just (Right (n1*n2)),ELitInt (n1*n2))
					else return (Int,Just (Right (n1*n2)),Neg (PMinus ((0,0),"-")) (ELitInt (-(n1*n2))))
		else error ("non-integer at times operator at line "++show(x)++", column "++show(y))	
			
checkExpTypeVal (EMul exp1 (Div (PDiv ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val2 of
			Nothing -> return (Int,Nothing,(EMul nexp1 (Div (PDiv ((x,y),"/"))) nexp2))
			Just (Right 0) -> error ("error divide by 0 at line "++show(x)++", column "++show(y))
			Just (Right n2) -> case val1 of 
				Nothing -> return (Int,Nothing,(EMul nexp1 (Div (PDiv ((x,y),"/"))) nexp2))
				Just (Right n1) -> if (div n1 n2)>=0
					then return (Int,Just (Right (div n1 n2)),ELitInt (div n1 n2))
					else return (Int,Just (Right (div n1 n2)),Neg (PMinus ((0,0),"-")) (ELitInt (-(div n1 n2))))
		else error ("non-integer at divide operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EMul exp1 (Mod (PMod ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val2 of
			Nothing -> return (Int,Nothing,(EMul nexp1 (Mod (PMod ((x,y),"%"))) nexp2))
			Just (Right 0) -> error ("error modulo by 0 at line "++show(x)++", column "++show(y))
			Just (Right n2) -> case val1 of 
				Nothing -> return (Int,Nothing,(EMul nexp1 (Mod (PMod ((x,y),"%"))) nexp2))
				Just (Right n1) -> if (mod n1 n2)>=0
					then return (Int,Just (Right (mod n1 n2)),ELitInt (mod n1 n2))
					else return (Int,Just (Right (mod n1 n2)),Neg (PMinus ((0,0),"-")) (ELitInt (-(mod n1 n2))))
		else error ("non-integer at modulo operator at line "++show(x)++", column "++show(y))		

checkExpTypeVal (EAdd exp1 (Plus (PPlus ((x,y),_))) exp2) = do							--gdyby nie było potrzeby braku łączności można by upraszczać wyrażenia typu x+1+1
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if type1==type2
		then case type1 of
			Int -> case val1 of
				Nothing -> return (Int,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),"+"))) nexp2))
				Just (Right n1) -> case val2 of 
					Nothing -> return (Int,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),"+"))) nexp2))
					Just (Right n2) -> if n1+n2>=0
						then return (Int,Just (Right (n1+n2)),ELitInt (n1+n2))
						else return (Int,Just (Right (n1+n2)),Neg (PMinus ((0,0),"-")) (ELitInt (-(n1+n2))))
			Str -> case val1 of
				Nothing -> return (Str,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),"+s"))) nexp2))
				Just (Left (Left s1)) -> case val2 of 
					Nothing -> return (Str,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),"+s"))) nexp2))
					Just (Left (Left s2)) -> return (Str,Just (Left (Left (s1++s2))),EString (s1++s2))
			_ -> error ("wrong type at plus operator at line "++show(x)++", column "++show(y))
		else error ("not matching types at plus operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EAdd exp1 (Minus (PMinus ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Int,Nothing,(EAdd nexp1 (Minus (PMinus ((x,y),"-"))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Int,Nothing,(EAdd nexp1 (Minus (PMinus ((x,y),"-"))) nexp2))
				Just (Right n2) -> if n1-n2>=0
					then return (Int,Just (Right (n1-n2)),ELitInt (n1-n2))
					else return (Int,Just (Right (n1-n2)),Neg (PMinus ((0,0),"-")) (ELitInt (-(n1-n2))))
		else error ("non-integer at minus operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (LTH (PLTH ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (LTH (PLTH ((x,y),"<"))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing,(ERel nexp1 (LTH (PLTH ((x,y),"<"))) nexp2))
				Just (Right n2) -> if n1<n2
					then return (Bool,Just (Left (Right True)),ELitTrue)
					else return (Bool,Just (Left (Right False)),ELitFalse)
		else error ("non-integer at < operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (LE (PLE ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (LE (PLE ((x,y),"<="))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing,(ERel nexp1 (LE (PLE ((x,y),"<="))) nexp2))
				Just (Right n2) -> if n1<=n2
					then return (Bool,Just (Left (Right True)),ELitTrue)
					else return (Bool,Just (Left (Right False)),ELitFalse)
		else error ("non-integer at <= operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (ERel exp1 (GTH (PGTH ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (GTH (PGTH ((x,y),">"))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing,(ERel nexp1 (GTH (PGTH ((x,y),">"))) nexp2))
				Just (Right n2) -> if n1>n2
					then return (Bool,Just (Left (Right True)),ELitTrue)
					else return (Bool,Just (Left (Right False)),ELitFalse)
		else error ("non-integer at > operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (GE (PGE ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (GE (PGE ((x,y),">="))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing,(ERel nexp1 (GE (PGE ((x,y),">="))) nexp2))
				Just (Right n2) -> if n1>=n2
					then return (Bool,Just (Left (Right True)),ELitTrue)
					else return (Bool,Just (Left (Right False)),ELitFalse)
		else error ("non-integer at >= operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (EQU (PEQU ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if type1==type2
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (EQU (PEQU ((x,y),"=="++(show type1)))) nexp2))
			Just _ -> case val2 of
				Nothing -> return (Bool,Nothing,(ERel nexp1 (EQU (PEQU ((x,y),"=="++(show type1)))) nexp2))
				Just _ -> if val1==val2
					then return (Bool,Just (Left (Right True)),ELitTrue)
					else return (Bool,Just (Left (Right False)),ELitFalse)
		else error ("different types at == operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (ERel exp1 (NE (PNE ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if type1==type2
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (EQU (PEQU ((x,y),"!="++(show type1)))) nexp2))
			Just _ -> case val2 of
				Nothing -> return (Bool,Nothing,(ERel nexp1 (EQU (PEQU ((x,y),"!="++(show type1)))) nexp2))
				Just _ -> if val1/=val2
					then return (Bool,Just (Left (Right True)),ELitTrue)
					else return (Bool,Just (Left (Right False)),ELitFalse)
		else error ("different types at != operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EAnd exp1 (PAnd ((x,y),_)) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Bool) && (type2==Bool))
		then case val1 of
			Just (Left (Right False)) -> return (Bool,Just (Left (Right False)),ELitFalse)
			Just (Left (Right True)) -> return (Bool,val2,nexp2)	
			Nothing -> case val2 of
				Just (Left (Right False)) -> return (Bool,Just (Left (Right False)),ELitFalse)
				Just (Left (Right True)) -> return (Bool,val1,nexp1)
				Nothing -> return (Bool,Nothing,(EAnd nexp1 (PAnd ((x,y),"&&")) nexp2))
		else error ("non-boolean at and operator at line "++show(x)++", column "++show(y))		

checkExpTypeVal (EOr exp1 (POr ((x,y),_)) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Bool) && (type2==Bool))
		then case val1 of
			Just (Left (Right True)) -> return (Bool,Just (Left (Right True)),ELitTrue)
			Just (Left (Right False)) -> return (Bool,val2,nexp2)		
			Nothing -> case val2 of
				Just (Left (Right True)) -> return (Bool,Just (Left (Right True)),ELitTrue)
				Just (Left (Right False)) -> return (Bool,val1,nexp1)			
				Nothing -> return (Bool,Nothing,(EOr nexp1 (POr ((x,y),"||")) nexp2))
		else error ("non-boolean at or operator at line "++show(x)++", column "++show(y))	
