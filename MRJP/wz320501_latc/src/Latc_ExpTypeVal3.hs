module Latc_ExpTypeVal where


--TODOTODOTODOTODO - rozgrzebane, nie skończone

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





checkExpTypeVal :: Expr -> StEnv (Type,Val,Expr)			-- oprócz Val też poprawiony Expr dając uproszczone wyrażenie

checkExpTypeVal (EVar (PIdent ((x,y),name))) = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++name++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Fun _ _,_) -> error ("error in line "++show(x)++", column "++show(y)++" attempt to use function "++name++" declared in line "++show(x1)++", column "++show(y1)++"as variable")	
			Just (t,val) -> case val of
				Nothing -> (t,val,(EVar (PIdent ((x,y),name))))
				Just (Right n) -> if n>=0 
					then (t,val,ELitInt n)
					else (t,val,Neg (PNeg ((0,0),"-")) (ElitInt (-n)))	--TODO być może returny
				Just (Left (Right True)) -> (t,val,ELitTrue)
				Just (Left (Right False)) -> (t,val,ELitFalse)
				Just (Left (Left str)) -> (t,val,(EString str))
			--Nothing -> error "???"

checkExpTypeVal (ELitInt n) = return (Int,Just (Right n),ELitInt n)
checkExpTypeVal ELitTrue = return (Bool,Just (Left (Right True)),ELitTrue)
checkExpTypeVal ELitFalse = return (Bool,Just (Left (Right False)),ELitFalse)

checkExpTypeVal (EApp (PIdent ((x,y),name)) exps) = do				--TODOTODO exps mogą się zmienić
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" function "++name++" does not exist")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just ((Fun t tlist),_) -> do
				multipleTypesMatch (PIdent ((x,y),name)) (x1,y1) exps tlist 1
				return (t,Nothing,(EApp (PIdent ((x,y),name)) exps))
			Just _ -> error ("error in line "++show(x)++", column "++show(y)++"attempt to use variable declared in line "++show(x1)++", column "++show(y1)++" as function")

checkExpTypeVal (EString str) = return (Str,Just (Left (Left str)),(EString str))

checkExpTypeVal (Neg (PNeg ((x,y),_)) exp) = do
	(type1,val,nexp) <- checkExpTypeVal exp
	if type1==Int
		then case val of
			Nothing -> return (Int,Nothing,(Neg (PNeg ((x,y),"-")) nexp))
			Just (Right n) -> return (if (-n)>=0 
					then (Int,Just (Right (-n)),ELitInt n)
					else (Int,Just (Right (-n)),Neg (PNeg ((x,y),"-")) (ElitInt (-n)))
			--Just _ -> error "???"
		else error ("error integer negation of non-integer expresion at line "++show(x)++", column "++show(y))

checkExpTypeVal (Not (PNot ((x,y),_)) exp) = do
	(type1,val,nexp) <- checkExpTypeVal exp
	if type1==Bool
		then case val of
			Nothing -> return (Bool,Nothing,(Not (PNot ((x,y),_)) nexp))
			Just (Left(Right True)) -> return (Bool,Just(Left (Right False)),ELitFalse)
			Just (Left(Right False)) -> return (Bool,Just(Left (Right True)),ELitTrue)
		else error ("error boolean negation of non-boolean expresion at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (EMul exp1 (Times (PTimes ((x,y),_))) exp2) = do		--TODO można wypisywać z której strony operatora jest błąd jak tylko z jednej
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Int,Nothing,(EMul nexp1 (Times (PTimes ((x,y),"*"))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Int,Nothing,(EMul nexp1 (Times (PTimes ((x,y),"*"))) nexp2))
				Just (Right n2) -> if n1*n2>=0
					then (Int,Just (Right (n1*n2)),ELitInt (n1*n2))
					else (Int,Just (Right (n1*n2)),Neg (PNeg ((0,0),"-")) (ElitInt (-(n1*n2))))
		else error ("error non-integer at times operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (EMul exp1 (Div (PDiv ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val2 of
			Nothing -> return (Int,Nothing,(EMul nexp1 (Div (PDiv ((x,y),_))) nexp2))
			Just (Right 0) -> error ("error divide by 0 at line "++show(x)++", column "++show(y))
			Just (Right n2) -> case val1 of 
				Nothing -> return (Int,Nothing,(EMul nexp1 (Div (PDiv ((x,y),_))) nexp2))
				Just (Right n1) -> if n1/n2>=0
					then (Int,Just (Right (div n1 n2)),ELitInt (n1/n2))
					else (Int,Just (Right (div n1 n2)),Neg (PNeg ((0,0),"-")) (ElitInt (-(n1/n2))))
		else error ("error non-integer at divide operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EMul exp1 (Mod (PMod ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val2 of
			Nothing -> return (Int,Nothing,(EMul nexp1 (Mod (PMod ((x,y),_))) nexp2))
			Just (Right 0) -> error ("error modulo by 0 at line "++show(x)++", column "++show(y))
			Just (Right n2) -> case val1 of 
				Nothing -> return (Int,Nothing,(EMul nexp1 (Mod (PMod ((x,y),_))) nexp2))
				Just (Right n1) -> if n1%n2>=0
					then (Int,Just (Right (mod n1 n2)),ELitInt (n1%n2))
					else (Int,Just (Right (mod n1 n2)),Neg (PNeg ((0,0),"-")) (ElitInt (-(n1%n2))))
		else error ("non-integer at modulo operator at line "++show(x)++", column "++show(y))		
		
checkExpTypeVal (EAdd exp1 (Plus (PPlus ((x,y),_))) exp2) = do
	(type1,val1,nexp) <- checkExpTypeVal exp1
	(type2,val2,nexp) <- checkExpTypeVal exp2
	if type1==type2
		then case type1 of
			Int -> case val1 of
				Nothing -> return (Int,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),_))) nexp2))
				Just (Right n1) -> case val2 of 
					Nothing -> return (Int,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),_))) nexp2))
					Just (Right n2) -> if n1+n2>=0
						then (Int,Just (Right (n1+n2)),ELitInt (n1+n2))
						else (Int,Just (Right (n1+n2)),Neg (PNeg ((0,0),"-")) (ElitInt (-(n1+n2))))
			Str -> case val1 of
				Nothing -> return (Str,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),_))) nexp2))
				Just (Left (Left s1)) -> case val2 of 
					Nothing -> return (Str,Nothing,(EAdd nexp1 (Plus (PPlus ((x,y),_))) nexp2))
					Just (Left (Left s2)) -> return (Int,Just (Left (Left (s1++s2))),EString (s1++s2))
			_ -> error ("wrong type at plus operator at line "++show(x)++", column "++show(y))
		else error ("not matching types at plus operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EAdd exp1 (Minus (PMinus ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Int,Nothing,(EAdd nexp1 (Minus (PMinus ((x,y),_))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Int,Nothing,(EAdd nexp1 (Minus (PMinus ((x,y),_))) nexp2))
				Just (Right n2) -> if n1-n2>=0
					then (Int,Just (Right (n1-n2)),ELitInt (n1-n2))
					else (Int,Just (Right (n1-n2)),Neg (PNeg ((0,0),"-")) (ElitInt (-(n1-n2))))
		else error ("non-integer at minus operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (LTH (PLTH ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (LTH (PLTH ((x,y),_))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing(ERel nexp1 (LTH (PLTH ((x,y),_))) nexp2))
				Just (Right n2) -> if n1<n2
					then (Bool,Just (Left (Right True)),ELitTrue)
					else (Bool,Just (Left (Right False)),ELitFalse)
		else error ("non-integer at < operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (LE (PLE ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing,(ERel nexp1 (LE (PLE ((x,y),_))) nexp2))
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing,(ERel nexp1 (LE (PLE ((x,y),_))) nexp2))
				Just (Right n2) -> if n1<=n2
					then (Bool,Just (Left (Right True)),ELitTrue)
					else (Bool,Just (Left (Right False)),ELitFalse)
		else error ("non-integer at <= operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (ERel exp1 (GTH (PGTH ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing)
				Just (Right n2) -> return (Bool,Just (Left (Right (n1>n2))))
		else error ("non-integer at > operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (GE (PGE ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Int) && (type2==Int))
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just (Right n1) -> case val2 of 
				Nothing -> return (Bool,Nothing)
				Just (Right n2) -> return (Bool,Just (Left (Right (n1>=n2))))
		else error ("non-integer at >= operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (ERel exp1 (EQU (PEQU ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if type1==type2
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just _ -> case val2 of
				Nothing -> return (Bool,Nothing)
				Just _ -> return (Bool,Just (Left (Right (val1==val2))))
		else error ("different types at == operator at line "++show(x)++", column "++show(y))
		
checkExpTypeVal (ERel exp1 (NE (PNE ((x,y),_))) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if type1==type2
		then case val1 of
			Nothing -> return (Bool,Nothing)
			Just _ -> case val2 of
				Nothing -> return (Bool,Nothing)
				Just _ -> return (Bool,Just (Left (Right (val1/=val2))))
		else error ("different types at != operator at line "++show(x)++", column "++show(y))

checkExpTypeVal (EAnd exp1 (PAnd ((x,y),_)) exp2) = do		--TODO można spróbować leniwie, choć chyba nie ma sensu pomijać sprawdzania poprawności
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Bool) && (type2==Bool))
		then case val1 of
			Just (Left (Right False)) -> return (Bool,Just (Left (Right False)))
			Just (Left (Right True)) -> return (Bool,val2)			
			Nothing -> case val2 of
				Just (Left (Right False)) -> return (Bool,Just (Left (Right False)))
				_ -> return (Bool,Nothing)
		else error ("non-boolean at and operator at line "++show(x)++", column "++show(y))		

checkExpTypeVal (EOr exp1 (POr ((x,y),_)) exp2) = do
	(type1,val1,nexp1) <- checkExpTypeVal exp1
	(type2,val2,nexp2) <- checkExpTypeVal exp2
	if ((type1==Bool) && (type2==Bool))
		then case val1 of
			Just (Left (Right True)) -> return (Bool,Just (Left (Right True)))
			Just (Left (Right False)) -> return (Bool,val2)			
			Nothing -> case val2 of
				Just (Left (Right True)) -> return (Bool,Just (Left (Right True)))
				_ -> return (Bool,Nothing)
		else error ("non-boolean at or operator at line "++show(x)++", column "++show(y))	
