module Latc_Code4 where

import Control.Monad.Reader
import Control.Monad.State
import System.Environment
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs
import Latc_basic

type Code4Function = ([Type],String,[Code4Block],Int,Int,[(String,Int)],Int)	--argumenty,nazwa,kod,ilość zmiennych,max tempów,lista stringów,max liczba argumentów w wywołaniu

type Code4Block = (String,[Code4Instruction])

data Code4Instruction =
	Empty4 ValVar4 |
	Ass4 ValVar4 ValVar4|
	OpV Op ValVar4 ValVar4 ValVar4 |
	Neg4 ValVar4 |
	Not4 ValVar4 |
	AllocArr14 ValVar4 ValVar4 ValVar4| --wskażnik , rozmiar, długość
	AllocArr24 ValVar4 ValVar4 ValVar4| --wskażnik , długość, default
	AllocObj4 ValVar4 Int| --powinno być wywołanie konstruktora, który wylicza i wprowadza wartości domyślne na właściwe miejsca, i zwraca wskaźnik do nowego obiektu
	Param4 Int ValVar4 |
	CallV ValVar4 String Int |
	Return4 ValVar4	|
	Goto4 String |
	If4 ValVar4 String
    deriving (Eq,Ord,Show)
  
data Op =
	Add4 |
	Add24 |
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
	SetNE4S
    deriving (Eq,Ord,Show)

data ValVar4 =
	Int4 Integer |
	Bool4 Bool |
	String4 Int |
	ArrElem4 ValVar4 Type ValVar4 |
	ObjAttr4 ValVar4 Type Int | 
	Pointer4 Int |
	Temp4 Int Type |
	Var4 Int Type |
	Rej4 |
	Void4 
    deriving (Eq,Ord,Show)

--środowisko zawiera mapę z nazw zmiennych w lokację i typ
type Env4 = Map.Map String (Int,Type)
type St4 = (String,Int,String,Int,Int,Set.Set Int,Int,Map.Map String Int,Int,Map.Map (String,String) (Type,Int), [Code4Block],[Code4Instruction])
--stan zawiera nazwę funkcji, numer ostatniego labelu, nazwę bloku aktualnie zapisywanego, lokację ostatniej zmiennej,maksymalną ilość tempów używanych,
--zbiór tempów zwolnionych, ilość stringów już zapisanych i ich mapowanie w etykiety, maksymalną ilość argumentów funkcji wywoływanych,
--pozycje atrybutów w objektach,zapisane już bloki proste funkcji i instrukcje z aktualnie zapisywanego bloku

type StEnv4 = ReaderT Env4 (State St4)

--funkcje zmieniające i czytające stan i środowisko
getTemp :: StEnv4 Int
getTemp = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	if (Set.null tempset)
		then do put (name,labels,nextlabel,vars,temps+1,tempset,strs,strmap,params,objattrs,blocks,instrs)
			return (temps+1)
		else do let (x,set2) = Set.deleteFindMin tempset
			put (name,labels,nextlabel,vars,temps,set2,strs,strmap,params,objattrs,blocks,instrs)
			return x

freeTemp :: Int -> StEnv4 ()
freeTemp x = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get	
	put (name,labels,nextlabel,vars,temps,Set.insert x tempset,strs,strmap,params,objattrs,blocks,instrs)

freeTemp2 :: ValVar4 -> StEnv4 ()
freeTemp2 (Temp4 x _) = freeTemp x
freeTemp2 _ = return ()

freeGetTemp :: ValVar4 -> ValVar4 -> StEnv4 Int
freeGetTemp x y = do
	freeTemp2 x
	freeTemp2 y
	getTemp


multiFreeTemp :: [ValVar4] -> StEnv4 ()
multiFreeTemp (x:xs) = do
	freeTemp2 x
	multiFreeTemp xs
multiFreeTemp [] = return ()
	

newVar :: Type -> StEnv4 Int
newVar t = do
	let size = valSize t
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	put (name,labels,nextlabel,vars+size,temps,tempset,strs,strmap,params,objattrs,blocks,instrs)
	return (vars+size)

addInstructions :: [Code4Instruction] -> StEnv4 ()
addInstructions instrs2 = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs1) <- get
	put (name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs1++instrs2)

writeBlock :: StEnv4 [Code4Block]
writeBlock = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	put (name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks ++ [(nextlabel,instrs)],[])
	return (blocks ++ [(nextlabel,instrs)])

getLabel :: StEnv4 String
getLabel = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	put (name,labels+1,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs)
	return (name++"_"++(show labels))

setLabel :: String -> StEnv4 ()
setLabel labelname = do
	(name,labels,_,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	put (name,labels,labelname,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs)

endLabel :: StEnv4 String
endLabel = do
	(name,_,_,_,_,_,_,_,_,_,_,_) <- get
	return (name++"_END")
	
pushString :: String -> StEnv4 Int
pushString str = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	case Map.lookup str strmap of
		Just n -> return n
		Nothing -> do
			put (name,labels,nextlabel,vars,temps,tempset,strs+1,Map.insert str (strs+1) strmap,params,objattrs,blocks,instrs)
			return (strs+1)

maxParams :: Int -> StEnv4 ()
maxParams n = do
	(name,labels,nextlabel,vars,temps,tempset,strs,strmap,params,objattrs,blocks,instrs) <- get
	if (n>params)
		then put (name,labels,nextlabel,vars,temps,tempset,strs,strmap,n,objattrs,blocks,instrs)
		else return ()

clAttrs :: String -> String -> StEnv4 (Type,Int)
clAttrs name attr = do
	(_,_,_,_,_,_,_,_,_,objattrs,_,_) <- get
	case Map.lookup (name,attr) objattrs of
		Just x -> return x

--rezerwacja i zwolnienie rezerwacji rejestrów które mogą zmieniać wartości w funkcji wołanej
reserveTemps :: StEnv4 ()
reserveTemps = do
	getTemp
	getTemp
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
	freeTemp 6
	freeTemp 7

--czy wyrażenie zawiera wywołanie funkcji - wtedy nie należy używać w nim rejestrów przekazujących argumenty
containsApp :: Expr -> Bool
containsApp (EApp _ _) = True
containsApp (EAdd _ (Plus (PPlus (_,"+s"))) _) = True
containsApp (Neg _ exp) = containsApp exp
containsApp (Not _ exp) = containsApp exp
containsApp (EMul exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (EAdd exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (ERel exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (EOr exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (EAnd exp1 _ exp2) = (containsApp exp1)||(containsApp exp2)
containsApp (ENewObj _ _) = True
containsApp (ENewArr _ _ _) = True
containsApp (EArrInd _ exp) = containsApp exp
containsApp _ = False	

----------------------------------------------------------------------------------------------------------------------------------------------

toCode4Ass :: ValVar4 -> Code4Instruction -> Code4Instruction
toCode4Ass var (OpV op (Temp4 _ _) x1 x2) = (OpV op var x1 x2)
toCode4Ass var (CallV _ name n) = CallV var name n
toCode4Ass var (Empty4 x) = Ass4 var x
toCode4Ass var (Ass4 (Temp4 _ _) y) = Ass4 var y
toCode4Ass var (AllocArr14 (Temp4 temp _) x y)= AllocArr14 var x y
toCode4Ass var (AllocArr24 (Temp4 temp _) x y)= AllocArr24 var x y

------------------------------------------------------------------------------------------------------------

toCode4ClAttr2 :: String -> Type -> [Item] -> Int -> ((Map.Map (String,String) (Type,Int)),Int)
toCode4ClAttr2 name t ((Init (PIdent (_,attr)) exp):its) n =
	let (map1,n1) = toCode4ClAttr2 name t its (n+(valSize t))
	in ((Map.insert (name,attr) (t,n) map1),n1)
toCode4ClAttr2 _ _ [] n = (Map.empty,n)

toCode4ClAttr1 :: String -> [InDef] -> Int -> ((Map.Map (String,String) (Type,Int)),Int)
toCode4ClAttr1 name ((AttrDef t attrs):ls) n = 
	let (map1,n1) = toCode4ClAttr2 name t attrs n
	in let (map2,n2) = toCode4ClAttr1 name ls n1
	in (Map.union map1 map2,n2)
toCode4ClAttr1 _ [] n = (Map.empty,n)


toCode4ClAttrsOffs :: [TopDef] -> (Map.Map (String,String) (Type,Int),Map.Map String Int)
toCode4ClAttrsOffs ((FnDef _ (PIdent (_,name)) args (Block bl)):fs) = toCode4ClAttrsOffs fs
toCode4ClAttrsOffs ((ClDef (PIdent ((x,y),name)) ls):fs) =
	let (map1,n) = toCode4ClAttr1 name ls 0
	in let (map2,map3) = toCode4ClAttrsOffs fs
	in (Map.union map1 map2, Map.insert name n map3)
toCode4ClAttrsOffs [] = (Map.empty,Map.empty)

-------------------------------------------------------------------------------------------------

toCode4ClConstr2 :: String -> Type -> [Item] -> StEnv4 ()
toCode4ClConstr2 name t ((Init (PIdent (_,attr)) exp):its) = do
	(r,temp) <- toCode4Expr2 exp
	(_,attrnum) <- clAttrs name attr
	if (null r)
		then do addInstructions [toCode4Ass (ObjAttr4 (Var4 8 (Class (PIdent ((0,0),name)))) t attrnum) (Empty4 temp)]	--przypisanie we właściwe miejsce
		else do addInstructions (r++[Ass4 (ObjAttr4 (Var4 8 (Class (PIdent ((0,0),name)))) t attrnum) temp])
		--((init r) ++[toCode4Ass (ObjAttr4 (Var4 8 (Class (PIdent ((0,0),name)))) t attrnum) (last r)])
	freeTemp2 temp
	toCode4ClConstr2 name t its
toCode4ClConstr2 _ _ [] = return ()


toCode4ClConstr1 :: String -> [InDef] -> StEnv4 ()
toCode4ClConstr1 name ((AttrDef t attrs):ls) = do
	toCode4ClConstr2 name t attrs
	toCode4ClConstr1 name ls
toCode4ClConstr1 _ [] = return ()


toCode4ClConstructor :: String -> [InDef] -> (Map.Map String Int)-> StEnv4 Code4Function
toCode4ClConstructor name ls map = do
	case Map.lookup name map
		of Just size -> do
			addInstructions [AllocObj4 (Var4 8 (Class (PIdent ((0,0),name)))) size]
			toCode4ClConstr1 name ls
			label <- endLabel			
			addInstructions [Return4 (Var4 8 (Class (PIdent ((0,0),name)))),Goto4 label]
			writeBlock
			(_,_,_,vars,temps,_,_,strmap,params,_,bl4,inst4) <- get
			let strList = List.sortBy (\(x1,y1) (x2,y2) -> (compare y1 y2)) (Map.toList strmap)
			return ([],"class_"++name,bl4++[(label,[])],vars,temps,strList,params)

-------------------------------------------------------------------------------------------------------------------------------------------------------------



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
	maxParams (length exps)
	if (name=="main") || (elem name predefinedNames)
		then return (str++[CallV (Temp4 temp (fromVarType x)) name (length exps)],(Temp4 temp (fromVarType x)))
		else return (str++[CallV (Temp4 temp (fromVarType x)) ("_"++name) (length exps)],(Temp4 temp (fromVarType x)))

toCode4Expr (ENewObj _ (PIdent (_,clname))) = do
	temp <- getTemp
	return ([CallV (Temp4 temp (Class (PIdent ((0,0),clname)))) ("class_"++clname) 0],(Temp4 temp (Class (PIdent ((0,0),clname)))))

toCode4Expr (ENewArr _ t exp) = do
	(str,x1) <- toCode4Expr exp
	freeTemp2 x1
	temp <- getTemp
	case t of
		Str -> do
			n <- pushString "" 
			return (str++[AllocArr24 (Temp4 temp (Array t)) x1 (String4 n)],(Temp4 temp (Array t)))
		_ ->return (str++[AllocArr14 (Temp4 temp (Array t)) (Int4 (toInteger (valSize t))) x1],(Temp4 temp (Array t)))
	--TODO jeśli zrobię struckty, to może tutaj zmienić tę instrukcję - tak żeby to tutaj dodać długość na stertę

	
toCode4Expr (EArrInd (PIdent (_,arrname)) exp) = do
	env <- ask
	(str,temp1) <- toCode4Expr exp
	temp2 <- getTemp
	freeTemp2 temp1
	case Map.lookup arrname env of
		Just (varnum, Array type1) -> return (str++[OpV Mul4 (Temp4 temp2 Int) temp1 (Int4 (toInteger (valSize type1))),OpV Add4 (Temp4 temp2 Int) (Temp4 temp2 Int) (Int4 4),
													Ass4 (Temp4 temp2 type1) (ArrElem4 (Var4 varnum (Array type1)) type1 (Temp4 temp2 Int))],(Temp4 temp2 type1))

toCode4Expr (EAttr (PIdent (_,name)) (PIdent (_,attr))) = do
	env <- ask
	temp <- getTemp
	case Map.lookup name env of
		Just (varnum, Array t) -> return ([Ass4 (Temp4 temp Int) (ArrElem4 (Var4 varnum (Array t)) Int (Int4 0))],(Temp4 temp Int))	--jak nie length to frontend nie przepuści
		Just (varnum, (Class (PIdent (_,clname)))) -> do
			(type1,offset) <- clAttrs clname attr
			return ([Ass4 (Temp4 temp type1) (ObjAttr4 (Var4 varnum (Class (PIdent ((0,0),clname)))) type1 offset)],Temp4 temp type1)
			
toCode4Expr (ENull t _) = return ([],Pointer4 0)

toCode4Expr (EString str) = do
	n <- pushString str
	return ([],String4 n)

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
	
toCode4Expr (ERel exp1 (EQU (PEQU _)) exp2) = do
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
	
toCode4Expr (ERel exp1 (NE (PNE _)) exp2) = do
	(str1,x1) <- toCode4Expr exp1
	(str2,x2) <- toCode4Expr exp2	
	temp <- freeGetTemp x1 x2
	return (str1++str2++[OpV SetNE4S (Temp4 temp Bool) x1 x2],(Temp4 temp Bool))
	
toCode4Expr (EOr exp1 (POr _) exp2) = do
	(inst1,x1) <- toCode4Expr exp1
	truelabel <- getLabel
	falselabel <- getLabel
	endlabel <- getLabel
	addInstructions (inst1 ++ [If4 x1 truelabel,Goto4 falselabel])
	freeTemp2 x1
	writeBlock
	setLabel truelabel
	addInstructions [Ass4 Rej4 (Bool4 True),Goto4 endlabel]
	writeBlock
	setLabel falselabel	
	(inst2,x2) <- toCode4Expr exp2
	addInstructions (inst2 ++ [Ass4 Rej4 x2,Goto4 endlabel])
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
	addInstructions (inst1 ++ [If4 x1 truelabel,Goto4 falselabel])
	freeTemp2 x1
	writeBlock
	setLabel truelabel	
	(inst2,x2) <- toCode4Expr exp2
	addInstructions (inst2 ++ [Ass4 Rej4 x2,Goto4 endlabel])
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
			x <- toCode4Expr exp
			unreserveTemps
			return x
		else toCode4Expr exp

toCode4Decl :: Type -> [Item] -> StEnv4 (Env4,[Code4Instruction])
toCode4Decl t ((Init (PIdent (_,varname)) exp):its) = do
	(r,temp) <- toCode4Expr2 exp
	varnum <- newVar t
	let str = if (null r)
		then [toCode4Ass (Var4 varnum t) (Empty4 temp)]
		else (init r) ++[toCode4Ass (Var4 varnum t) (last r)]
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
				addInstructions [toCode4Ass (Var4 varnum type1) (Empty4 temp)]
				ask
			else do
				addInstructions ((init inst4) ++[toCode4Ass (Var4 varnum type1) (last inst4)])
				ask

toCode4Stmt (ArrAss (PIdent (_,arrname)) exp1 exp2) = do
	env <- ask
	(inst1,temp1) <- toCode4Expr2 exp1
	(inst2,temp2) <- toCode4Expr2 exp2
	freeTemp2 temp1
	temp <- getTemp
	freeTemp temp
	freeTemp2 temp2
	case Map.lookup arrname env of
		Just (varnum, Array type1) -> do
			addInstructions (inst1++inst2++[OpV Mul4 (Temp4 temp Int) temp1 (Int4 (toInteger (valSize type1))),OpV Add4 (Temp4 temp Int) (Temp4 temp Int) (Int4 4),
											Ass4 (ArrElem4 (Var4 varnum (Array type1)) type1 (Temp4 temp Int)) temp2])
			ask


toCode4Stmt (AttrAss (PIdent (_,name)) (PIdent (_,attr)) exp) = do
	env <- ask
	(inst1,temp1) <- toCode4Expr2 exp
	freeTemp2 temp1
	case Map.lookup name env of
		Just (varnum, Class (PIdent (_,clname))) -> do
			(type1,offset) <- clAttrs clname attr
			addInstructions (inst1++[Ass4 (ObjAttr4 (Var4 varnum (Class (PIdent ((0,0),clname)))) type1 offset) temp1])
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
	(inst4,temp4) <- toCode4Expr2 exp
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

toCode4Stmt (ForEach _ t (PIdent (_,var)) (PIdent (_,arr)) stm) = do
	varnum1 <- newVar (Array t)
	varnum2 <- newVar (Array t)
	varnum3 <- newVar t
	env <- ask
	case Map.lookup arr env of
		Just (arrnum,Array type1) -> do
			addInstructions [OpV Add24 (Var4 varnum1 (Array t)) (Var4 arrnum (Array t)) (Int4 4)]	--TODOTODO to daje dodawanie na typach 8 bajt, a nie tylko na 4 bajt?
			temp <- getTemp
			addInstructions [Ass4 (Temp4 temp Int) (ArrElem4 (Var4 arrnum (Array t)) Int (Int4 0)),OpV Mul4 (Temp4 temp Int) (Temp4 temp Int) (Int4 (toInteger (valSize t))),
							OpV Add24 (Var4 varnum2 (Array t)) (Var4 varnum1 (Array t)) (Temp4 temp (Array t))]
			freeTemp temp
			condlabel <- getLabel
			addInstructions [Goto4 condlabel]
			writeBlock
			insidelabel <-getLabel
			endlabel <- getLabel
			setLabel insidelabel
			addInstructions [Ass4 (Var4 varnum3 type1) (ArrElem4 (Var4 varnum1 (Array t)) type1 (Int4 0))]
			(local (Map.insert var (varnum3,type1)) (toCode4Stmt stm))
			addInstructions [OpV Add24 (Var4 varnum1 (Array t)) (Var4 varnum1 (Array t)) (Int4 (toInteger (valSize t))),Goto4 condlabel]
			writeBlock
			setLabel condlabel
			temp <- getTemp
			addInstructions [OpV SetNE4S (Temp4 temp Bool) (Var4 varnum1 (Array t)) (Var4 varnum2 (Array t)),If4 (Temp4 temp Bool) insidelabel,Goto4 endlabel]
			freeTemp temp
			writeBlock
			setLabel endlabel
			ask

toCode4Stmt (SExp exp) = do
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
				(Arg t (PIdent (_,name))) -> if ((length args) >= 7)
					then (Map.insert name (-116-8*((length args)-7),t) env1,116+8*((length args)-7))
					else ((Map.insert name ((-1)*(valSize t)-offset,t) env1),offset+(valSize t))


toCode4Function :: TopDef -> (Map.Map (String,String) (Type,Int)) -> StEnv4 Code4Function
toCode4Function (FnDef _ (PIdent (_,name)) args (Block bl)) map1 = do
	if(name=="main")
		then put ("main",1,"main_0",0,0,Set.empty,0,Map.empty,0,map1,[],[])
		else put ("_"++name,1,"_"++name++"_0",0,0,Set.empty,0,Map.empty,0,map1,[],[])
	let (env4,_) = code4Arguments args
	(local (\x -> env4) (toCode4Block bl))
	label <- endLabel		
	addInstructions [Goto4 label]
	writeBlock
	(name2,_,_,vars,temps,_,_,strmap,params,_,bl4,inst4) <- get
	let strList = List.sortBy (\(x1,y1) (x2,y2) -> (compare y1 y2)) (Map.toList strmap)
	return (argTypes args,name2,bl4++[(label,[])],vars,temps,strList,params)


toCode4TopDef :: TopDef -> (Map.Map (String,String) (Type,Int)) -> (Map.Map String Int)-> Code4Function

toCode4TopDef (ClDef (PIdent (_,name)) ls) map1 map2 = 
	let (f,_) = runState (runReaderT (toCode4ClConstructor name ls map2) (Map.empty)) ("class_"++name,1,"class_"++name++"_0",8,0,Set.empty,0,Map.empty,0,map1,[],[])
	in f
	
toCode4TopDef (FnDef a b c d) map1 _=
	let (code4,_) = runState (runReaderT (toCode4Function (FnDef a b c d) map1) (Map.empty)) ("",0,"",0,0,Set.empty,0,Map.empty,0,Map.empty,[],[])
	in code4


	
toCode4Program :: [TopDef] -> (Map.Map (String,String) (Type,Int)) -> (Map.Map String Int)-> [Code4Function]
toCode4Program (f:fs) map1 map2 =
	let code41 = toCode4TopDef f map1 map2	--TODO ostatnie map.empty zamienić na wynik toCode4ClAttrsOffs
	in let code42 = toCode4Program fs map1 map2
	in code41:code42
	
toCode4Program [] _ _ = []


toCode4 :: [TopDef] -> [Code4Function]	--TODOTODO do Code4Program też trzeba dać map1
toCode4 fs =
	let (map1,map2) = toCode4ClAttrsOffs fs
	in (toCode4Program fs map1 map2)
