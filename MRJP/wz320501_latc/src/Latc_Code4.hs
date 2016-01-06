module Latc_Code4 where

import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set


import Latte.Abs
import Latte.Lex
import Latte.Par
import Latte.ErrM
import Latc_basic

type Code4Function = ([Type],String,[Code4Block],Int,Int)	--argumenty,nazwa,kod,l.zmiennych,max tempów

type Code4Block = (String,[Code4Instruction])

data Code4Instruction =
	Empty4 ValVar4 |
	Ass4 ValVar4 ValVar4|	--TODO tutaj też typ
	--OpE Op ValVar4 ValVar4 |
	OpV Op ValVar4 ValVar4 ValVar4 |
	Neg4 ValVar4 |
	Not4 ValVar4 |
	Param4 Int ValVar4 |
	--CallE String Integer |	
	CallV ValVar4 String Int |
	Return4 ValVar4	|
	Goto4 String |
	If4 ValVar4 String
    deriving (Eq,Ord,Show)
  
data Op =
	Add4 |
	Concat4 |
	Sub4 |
	Mul4 |
	Div4 |
	Mod4 |
	SetL4 |
	SetG4 |
	SetLE4 |
	SetGE4 |
	SetE4B |
	SetE4I |
	SetE4S |
	SetNE4B |
	SetNE4I |
	SetNE4S |
	Xor4
    deriving (Eq,Ord,Show)

data ValVar4 =
	Int4 Integer |
	Bool4 Bool |
	String4 String |
	Temp4 Int Type|
	Var4 Int Type|	--TODO można by pamiętać jezcze rozmiar/typ, a nawet trzeba - typ dodawani, przypisania itp. - 		
	Rej4 |
	Void4 
    deriving (Eq,Ord,Show)	

type Env4 = Map.Map String (Int,Type)
type St4 = (String,Int,String,Int,Int,Set.Set Int,[Code4Block],[Code4Instruction])	--nazwa, labele, zmienne, tempy

type StEnv4 = ReaderT Env4 (State St4)

--TODOTODOTODOTODOTODOTODOTODOTODO lepsze zarządzanie temp
--TODO lepsze kolokacje między blokami - graf przepływu do alokacji rejestrów i optymalizacji


--nextTemp :: StEnv4 Integer
--nextTemp = do
--	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs) <- get
--	put (name,labels,nextlabel,vars,temps+1,tempset,blocks,instrs)
--	return temps
	
getTemp :: StEnv4 Int
getTemp = do
	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs) <- get
	if (Set.null tempset)
		then do put (name,labels,nextlabel,vars,temps+1,tempset,blocks,instrs)
			return (temps+1)
		else do let (x,set2) = Set.deleteFindMin tempset
			put (name,labels,nextlabel,vars,temps,set2,blocks,instrs)
			return x

freeTemp :: Int -> StEnv4 ()
freeTemp x = do
	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs) <- get	
	put (name,labels,nextlabel,vars,temps,Set.insert x tempset,blocks,instrs)

freeTemp2 :: ValVar4 -> StEnv4 ()
freeTemp2 (Temp4 x _) = freeTemp x
freeTemp2 _ = return ()

freeGetTemp :: ValVar4 -> ValVar4 -> StEnv4 Int
freeGetTemp x y = do
	freeTemp2 x
	freeTemp2 y
	getTemp

--TODO to jest tylko tymczasowe - i tak trzeba tempty przepisać na właściwe rejestry lub stos
multiFreeTemp :: [ValVar4] -> StEnv4 ()
multiFreeTemp (x:xs) = do
	freeTemp2 x
	multiFreeTemp xs
multiFreeTemp [] = return ()
	

newVar :: Type -> StEnv4 Int
newVar t = do
	let size = valSize t
	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs) <- get
	put (name,labels,nextlabel,vars+size,temps,tempset,blocks,instrs)
	return (vars+size)

addInstructions :: [Code4Instruction] -> StEnv4 ()
addInstructions instrs2 = do
	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs1) <- get
	put (name,labels,nextlabel,vars,temps,tempset,blocks,instrs1++instrs2)

writeBlock :: StEnv4 [Code4Block]
writeBlock = do
	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs) <- get
	put (name,labels,nextlabel,vars,temps,tempset,blocks ++ [(nextlabel,instrs)],[])
	return (blocks ++ [(nextlabel,instrs)])

getLabel :: StEnv4 String
getLabel = do
	(name,labels,nextlabel,vars,temps,tempset,blocks,instrs) <- get
	put (name,labels+1,nextlabel,vars,temps,tempset,blocks,instrs)
	return (name++"_"++(show labels))

setLabel :: String -> StEnv4 ()
setLabel labelname = do
	(name,labels,_,vars,temps,tempset,blocks,instrs) <- get
	put (name,labels,labelname,vars,temps,tempset,blocks,instrs)

endLabel :: StEnv4 String
endLabel = do
	(name,_,_,_,_,_,_,_) <- get
	return (name++"_END")
	
--TODO w C jest przesunięcie o 7 a nie o 5 - niewiadomo dlaczego - może lepiej przsuwać o 7
reserveTemps :: StEnv4 ()
reserveTemps = do
	getTemp
	getTemp
	getTemp
	getTemp
	getTemp
	return ()
	
unreserveTemps :: StEnv4 ()
unreserveTemps = do
	freeTemp 1
	freeTemp 2
	freeTemp 3
	freeTemp 4
	freeTemp 5

containsApp :: Expr -> Bool
containsApp (EApp _ _) = True
containsApp (Neg _ exp) = containsApp exp
containsApp (Not _ exp) = containsApp exp
containsApp (EMul exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (EAdd exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (ERel exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (EOr exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (EAnd exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp _ = False	

--multiContainsApp :: [Expr] -> Bool
--multiContainsApp (exp:exps) = (containsApp exp)||(multiContainsApp exps)
--multiContainsApp [] = False

----------------------------------------------------------------------------------------------------------------------------------------------


toCode4App2 :: [Expr] -> Int -> StEnv4 ([Code4Instruction],[ValVar4],[Code4Instruction])
toCode4App2 (exp:exps) n = do
	(str,x) <- toCode4Expr exp
	(str2,xs,params) <- toCode4App2 exps (n+1)
	return (str++str2,x:xs,(Param4 n x):params)

toCode4App2 [] _= return ([],[],[])

toCode4App :: [Expr] -> StEnv4 ([Code4Instruction],[ValVar4])
toCode4App exps = do
	(str1,xs,params) <- toCode4App2 exps 1
	return (str1++params,xs)




toCode4Expr :: Expr -> StEnv4 ([Code4Instruction],ValVar4)

toCode4Expr (EVar (PIdent (_,x))) = do
	env <- ask 
	case Map.lookup x env of
		Just (varnum,type1) -> return ([],Var4 varnum type1)
toCode4Expr (ELitInt n) = return ([],Int4 n)
toCode4Expr ELitTrue = return ([],Bool4 True)
toCode4Expr ELitFalse = return ([],Bool4 False)

toCode4Expr (EApp (PIdent ((x,_),name)) exps) = do
	(str,xs) <- toCode4App exps
	multiFreeTemp xs
	temp <- getTemp
	return (str++[CallV (Temp4 temp (fromVarType x)) name (length exps)],(Temp4 temp (fromVarType x)))

toCode4Expr (EString str) = return ([],String4 str)

toCode4Expr (Neg (PMinus _) (ELitInt n)) = return([],Int4 (-n))

toCode4Expr (Neg (PMinus _) exp) = do
	(str,x) <- toCode4Expr exp
	case x of
		Temp4 _ _ -> return (str++[Neg4 x],x)
		_ -> do
			temp <- getTemp
			return (str++[Ass4 (Temp4 temp Int) x ,Neg4 (Temp4 temp Int)],(Temp4 temp Int ))

toCode4Expr (Not (PNot _) exp) = do
	(str,x) <- toCode4Expr exp
	case x of
		Temp4 _ _-> return (str++[Not4 x],x)
		_ -> do
			temp <- getTemp
			return (str++[Ass4 (Temp4 temp Bool) x ,Not4 (Temp4 temp Bool)],(Temp4 temp Bool))

toCode4Expr (EMul exp1 (Times (PTimes _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV Mul4 (Temp4 temp Int) x1 x2],(Temp4 temp Int))
	
toCode4Expr (EMul exp1 (Div (PDiv _)) exp2) = do	-- jeśli dzielimy przez stałą, to przypisać ją na temp,podzielić przez nią i zapomnieć temp
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2
	temp <- freeGetTemp x1 x2	
	case x2 of
		(Int4 n) -> do
			temp2 <- getTemp
			freeTemp temp2
			return (str1++str2++[Ass4 (Temp4 temp2 Int ) (Int4 n),OpV Div4 (Temp4 temp Int) x1 (Temp4 temp2 Int)],(Temp4 temp Int))
		_ -> return (str1++str2++[OpV Div4 (Temp4 temp Int) x1 x2],(Temp4 temp Int))
	
toCode4Expr (EMul exp1 (Mod (PMod _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	case x2 of
		(Int4 n) -> do
			temp2 <- getTemp
			freeTemp temp2
			return (str1++str2++[Ass4 (Temp4 temp2 Int ) (Int4 n),OpV Mod4 (Temp4 temp Int) x1 (Temp4 temp2 Int)],(Temp4 temp Int))
		_ -> return (str1++str2++[OpV Mod4 (Temp4 temp Int) x1 x2],(Temp4 temp Int))
	
toCode4Expr (EAdd exp1 (Plus (PPlus (_,"+"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV Add4 (Temp4 temp Int) x1 x2],(Temp4 temp Int))
	
toCode4Expr (EAdd exp1 (Plus (PPlus (_,"+s"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV Concat4 (Temp4 temp Str) x1 x2],(Temp4 temp Str))
	
toCode4Expr (EAdd exp1 (Minus (PMinus _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV Sub4 (Temp4 temp Int) x1 x2],(Temp4 temp Int))
	
toCode4Expr (ERel exp1 (LTH (PLTH _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetL4 (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))	
	
toCode4Expr (ERel exp1 (LE (PLE _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetLE4 (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))	
	
toCode4Expr (ERel exp1 (GTH (PGTH _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetG4 (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))	
	
toCode4Expr (ERel exp1 (GE (PGE _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetGE4 (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))

toCode4Expr (ERel exp1 (EQU (PEQU (_,"==Bool"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetE4B (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))	

toCode4Expr (ERel exp1 (EQU (PEQU (_,"==Int"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetE4I (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))	
	
toCode4Expr (ERel exp1 (EQU (PEQU (_,"==Str"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetE4S (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))	
	
toCode4Expr (ERel exp1 (NE (PNE (_,"!=Bool"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetNE4B (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))

toCode4Expr (ERel exp1 (NE (PNE (_,"!=Int"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetNE4I (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))
	
toCode4Expr (ERel exp1 (NE (PNE (_,"!=Str"))) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetNE4S (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))
	
toCode4Expr (EOr exp1 (POr _) exp2) = do
	(inst1,x1) <- toCode4Expr exp1
	truelabel <- getLabel
	falselabel <- getLabel
	endlabel <- getLabel
	addInstructions [If4 x1 truelabel,Goto4 falselabel]
	freeTemp2 x1
	writeBlock
	setLabel truelabel
	addInstructions [Ass4 Rej4 (Bool4 True),Goto4 endlabel]
	writeBlock
	setLabel falselabel	
	(inst2,x2) <- toCode4Expr exp2
	addInstructions [Ass4 Rej4 x2,Goto4 endlabel]
	freeTemp2 x2
	writeBlock
	setLabel endlabel
	temp <- getTemp
	return ([Ass4 (Temp4 temp Bool) Rej4],(Temp4 temp Bool))
	
toCode4Expr (EAnd exp1 (PAnd _) exp2) = do
	(inst1,x1) <- toCode4Expr exp1
	truelabel <- getLabel
	falselabel <- getLabel
	endlabel <- getLabel
	addInstructions [If4 x1 truelabel,Goto4 falselabel]
	freeTemp2 x1
	writeBlock
	setLabel truelabel	
	(inst2,x2) <- toCode4Expr exp2
	addInstructions [Ass4 Rej4 x2,Goto4 endlabel]
	freeTemp2 x2
	writeBlock	
	setLabel falselabel
	addInstructions [Ass4 Rej4 (Bool4 False),Goto4 endlabel]
	writeBlock
	setLabel endlabel
	temp<-getTemp
	return ([Ass4 (Temp4 temp Bool) Rej4],(Temp4 temp Bool))	

toCode4Expr2 :: Expr -> StEnv4 ([Code4Instruction],ValVar4)
toCode4Expr2 exp = do
	if (containsApp exp)
		then do
			reserveTemps
			x<- toCode4Expr exp
			unreserveTemps
			return x
		else toCode4Expr exp
		



toCode4Ass :: Int -> Type -> Code4Instruction -> Code4Instruction
toCode4Ass varnum t (OpV op (Temp4 _ _) x1 x2) = (OpV op (Var4 varnum t) x1 x2)
toCode4Ass varnum t (CallV _ name n) = CallV (Var4 varnum t) name n
toCode4Ass varnum t (Empty4 x) = Ass4 (Var4 varnum t) x
toCode4Ass varnum t (Ass4 (Temp4 _ _) Rej4) = Ass4 (Var4 varnum t) Rej4



toCode4Decl :: Type -> [Item] -> StEnv4 (Env4,[Code4Instruction])
toCode4Decl t ((Init (PIdent (_,varname)) exp):its) = do
	(r,temp) <- toCode4Expr2 exp
	varnum <- newVar t
	let str = if (null r)
		then [toCode4Ass varnum t (Empty4 temp)]
		else (init r) ++[toCode4Ass varnum t (last r)]
	freeTemp2 temp		
	(env4,r2) <- (local (Map.insert varname (varnum,t))  (toCode4Decl t its))
	return (env4,str++r2)

toCode4Decl _ [] = do
	env <- ask
	return (env,[])

toCode4Stmt :: Stmt -> StEnv4 Env4

toCode4Stmt Empty = ask

toCode4Stmt (BStmt (Block bl)) = do
	toCode4Block bl
	ask

toCode4Stmt (Decl t l) = do
	(env4,inst4) <- toCode4Decl t l
	addInstructions inst4
	return env4

toCode4Stmt (Ass (PIdent (_,varname)) exp) = do
	env <- ask
	(inst4,temp) <- toCode4Expr2 exp
	freeTemp2 temp
	case Map.lookup varname env of
		Just (varnum,type1) -> if (null inst4)
				then do
					addInstructions [toCode4Ass varnum type1 (Empty4 temp)]
					ask
				else do
					addInstructions ((init inst4) ++[toCode4Ass varnum type1 (last inst4)])
					ask

toCode4Stmt (Incr (PIdent (_,varname))) = do
	env <- ask
	case Map.lookup varname env of
		Just (varnum,type1) -> do
			addInstructions [OpV Add4 (Var4 varnum type1) (Var4 varnum type1) (Int4 1)]
			ask

toCode4Stmt (Decr (PIdent (_,varname))) = do
	env <- ask
	case Map.lookup varname env of
		Just (varnum,type1) -> do
			addInstructions [OpV Sub4 (Var4 varnum type1) (Var4 varnum type1) (Int4 1)]
			ask

toCode4Stmt (Ret _ exp) = do
	(inst4,temp) <-toCode4Expr2 exp
	label <- endLabel
	addInstructions (inst4 ++[Return4 temp,Goto4 label])
	freeTemp2 temp
	ask

toCode4Stmt (VRet _) = do
	label <- endLabel
	addInstructions [Goto4 label]
	ask

toCode4Stmt (Cond (PIf _) exp stm) = do
	condlabel <- getLabel
	addInstructions [Goto4 condlabel]
	writeBlock
	truelabel <- getLabel
	endlabel <- getLabel	
	setLabel truelabel
	toCode4Stmt stm	
	addInstructions [Goto4 endlabel]
	writeBlock		
	setLabel condlabel
	(inst4,temp4) <- toCode4Expr2 exp	-- warunek typu bool - może być zmienna lub temp	
	addInstructions inst4
	addInstructions [If4 temp4 truelabel,Goto4 endlabel]
	freeTemp2 temp4
	writeBlock	
	setLabel endlabel
	ask



toCode4Stmt (CondElse (PIf _) exp stm stm2) = do
	condlabel <- getLabel
	addInstructions [Goto4 condlabel]
	writeBlock
	truelabel <- getLabel
	falselabel <- getLabel
	endlabel <- getLabel
	setLabel truelabel
	toCode4Stmt stm	
	addInstructions [Goto4 endlabel]
	writeBlock
	setLabel falselabel
	toCode4Stmt stm2
	addInstructions [Goto4 endlabel]
	writeBlock	
	setLabel condlabel
	(inst4,temp4) <- toCode4Expr2 exp
	addInstructions inst4
	addInstructions [If4 temp4 truelabel,Goto4 falselabel]
	freeTemp2 temp4
	writeBlock	
	setLabel endlabel
	ask

toCode4Stmt (While (PWhile _) exp stm) = do
	condlabel <- getLabel
	addInstructions [Goto4 condlabel]
	writeBlock
	truelabel <- getLabel
	endlabel <- getLabel
	setLabel truelabel
	toCode4Stmt stm	
	addInstructions [Goto4 condlabel]
	writeBlock		
	setLabel condlabel	
	(inst4,temp4) <- toCode4Expr2 exp		
	addInstructions inst4
	addInstructions [If4 temp4 truelabel,Goto4 endlabel]
	freeTemp2 temp4
	writeBlock
	setLabel endlabel
	ask

toCode4Stmt (SExp exp) = do		--TODO można zamienić OpV na OpE
	(inst4,temp4) <- toCode4Expr2 exp
	addInstructions inst4
	freeTemp2 temp4
	ask



toCode4Block :: [Stmt] -> StEnv4 ()
toCode4Block (stm:stmts) = do
	env4 <- toCode4Stmt stm
	(local (\x -> env4) (toCode4Block stmts))
	
toCode4Block [] = return ()


code4Arguments :: [Arg] -> (Env4,Int)
code4Arguments args = 
	if(null args)
		then (Map.empty,0)
		else let (env1,offset) = code4Arguments (init args)
			in case (last args) of
				(Arg t (PIdent (_,name))) -> if ((length args) == 7)
					then (Map.insert name (-124,t) env1,124)
					else ((Map.insert name ((-1)*(valSize t)-offset,t) env1),offset+(valSize t))--TODOTODOTODOTODO czy to jest poprawne - czy rozmiar tego argumentu czy raczej poprzedniego i czy zawsze 24, czy też jest to zależne od wołającego


toCode4TopDef :: TopDef -> StEnv4 Code4Function
toCode4TopDef (FnDef _ (PIdent (_,name)) args (Block bl)) = do
	put (name,1,name++"_0",0,0,Set.empty,[],[])
	let (env4,_) = code4Arguments args
	(local (\x -> env4) (toCode4Block bl))
	--(_,_,_,vars,temps,_,bl4,inst4) <- get
	--if ((null inst4)&&(not (null bl4)))
		--then return (argTypes args,name,bl4,vars,temps)
		--else do
	label <- endLabel		
	addInstructions [Goto4 label]
	writeBlock
	--(_,_,_,_,_,_,bl4,_) <- get
	(_,_,_,vars,temps,_,bl4,inst4) <- get

	return (argTypes args,name,bl4++[(label,[])],vars,temps)

toCode4 :: [TopDef] -> [Code4Function]
toCode4 (f:fs) = 
	let (code41,_) = runState (runReaderT (toCode4TopDef f) (Map.empty)) ("",0,"",0,0,Set.empty,[],[])
	in let code42 = toCode4 fs
	in code41:code42
	
toCode4 [] = []
