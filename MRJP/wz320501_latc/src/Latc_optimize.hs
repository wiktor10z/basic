module Latc_optimize where

import Control.Monad.Reader
import Control.Monad.State
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs
import Latc_basic
import Latc_Code4

--usunięcie zbędnych goto po return
optGotoAftRet :: Code4Block -> Code4Block
optGotoAftRet (label,instrs) =
	if (length instrs) <= 1
		then (label,instrs)
	else case last (init instrs) of
		Goto4 _ -> (label,init instrs)
		_ -> (label,instrs)

--zmiana poprzedników po usunięciu jakiegoś bloku
optChangePreds :: (String,Set.Set String) -> ([Code4Instruction],[String],Set.Set String)->([Code4Instruction],[String],Set.Set String)
optChangePreds (name2,set2) (x,strs,set) = (x,strs,Set.delete name2 (Set.union set set2))

--zmiana następników po usunięciu jakiegoś bloku
optChangeSuccs :: (String,String) -> ([Code4Instruction],[String],Set.Set String)->([Code4Instruction],[String],Set.Set String)
optChangeSuccs (label,nlabel) (x,[oldlabel],set) = ((init x)++[Goto4 nlabel],[nlabel],set)
optChangeSuccs (label,nlabel) (x,[oldlabel1,oldlabel2],set) =
	if label==oldlabel1
		then case last (init x) of
			If4 a _ -> if oldlabel2 == nlabel
				then (init(init x)++[Goto4 nlabel],[nlabel],set)
				else (init(init x)++[If4 a nlabel]++[Goto4 oldlabel2],[nlabel,oldlabel2],set)
		else if oldlabel1 == nlabel
			then (init(init x)++[Goto4 nlabel],[nlabel],set)
			else ((init x)++[Goto4 nlabel],[oldlabel1,nlabel],set)


optChangeSuccsAll :: ([String],String,String) -> Map.Map String ([Code4Instruction],[String],Set.Set String)-> Map.Map String ([Code4Instruction],[String],Set.Set String)
optChangeSuccsAll ((elem:set),label,nlabel) map = 
	let map2 = Map.adjust (optChangeSuccs (label,nlabel)) elem map
	in optChangeSuccsAll (set,label,nlabel) map2
optChangeSuccsAll ([],_,_) map = map

-- usuwanie martwych bloków
optEraseEmptyBlock1 :: [(String,([Code4Instruction],[String],Set.Set String))] -> Map.Map String ([Code4Instruction],[String],Set.Set String) -> Map.Map String ([Code4Instruction],[String],Set.Set String)
optEraseEmptyBlock1 ((name,([Goto4 _],[label],set)):list) map = 
	let m2 = Map.delete name map
	in let m3 = Map.adjust (optChangePreds (name,set)) label m2
	in optChangeSuccsAll ((Set.toList set),name,label) m3
optEraseEmptyBlock1 ((_):list) map = optEraseEmptyBlock1 list map
optEraseEmptyBlock1 [] map = map


optEraseEmptyBlocks :: Map.Map String ([Code4Instruction],[String],Set.Set String) -> Map.Map String ([Code4Instruction],[String],Set.Set String) -- nie usuwa bloku 0 - specjalnie
optEraseEmptyBlocks map = optEraseEmptyBlock1 (tail (Map.toList map)) map


optEraseEmptyBlocksFix :: Map.Map String ([Code4Instruction],[String],Set.Set String) -> Map.Map String ([Code4Instruction],[String],Set.Set String)
optEraseEmptyBlocksFix map = 
	let map2 = optEraseEmptyBlocks map
	in if map==map2
		then map
		else optEraseEmptyBlocksFix map2

--wyliczenie następników bloków
optSucc :: [Code4Instruction] -> [String]
optSucc [] = []
optSucc [Goto4 label] = [label]
optSucc [x] = []
optSucc instrs = case (last instrs) of
	Goto4 label -> case last (init instrs) of
		If4 _ label2 -> [label2,label]
		_ -> [label]
	_ -> []


optSuccList :: [Code4Block] -> [(String,[String])]
optSuccList ((label,instrs):bs) = ((label,optSucc instrs):(optSuccList bs))
optSuccList [] = []

--wyliczanie poprzedników bloków
optToMapEmpty :: [Code4Block] -> Map.Map String ([Code4Instruction],[String],Set.Set String)
optToMapEmpty ((label,instrs):bs) = Map.insert label (instrs,optSucc instrs,Set.empty) (optToMapEmpty bs)
optToMapEmpty [] = Map.empty


optUpdateSet :: String -> ([Code4Instruction],[String],Set.Set String)->([Code4Instruction],[String],Set.Set String)
optUpdateSet s (x,y,set) =(x,y,Set.insert s set)


optToMapSet :: [(String,[String])] -> Map.Map String ([Code4Instruction],[String],Set.Set String) -> Map.Map String ([Code4Instruction],[String],Set.Set String)

optToMapSet ((s,[]):ls) m = optToMapSet ls m

optToMapSet ((s,[label]):ls) m =
	let m2 = Map.adjust (optUpdateSet s) label m
	in optToMapSet ls m2
	
optToMapSet ((s,[label,label2]):ls) m =
	let m2 = Map.adjust (optUpdateSet s) label m
	in let m3 = Map.adjust (optUpdateSet s) label2 m2
	in optToMapSet ls m3
	
optToMapSet [] m = m

-- zamiana listy bloków prostych w graf przepływu i z powrotem
optToMap :: [Code4Block] -> Map.Map String ([Code4Instruction],[String],Set.Set String)
optToMap bs = optToMapSet (optSuccList bs) (optToMapEmpty bs)


optFromMap :: [(String,([Code4Instruction],[String],Set.Set String))] -> [Code4Block]
optFromMap ((label,(instrs,_,_)):ls) = ((label,instrs):(optFromMap ls))
optFromMap [] = []


optimize :: Code4Function -> Code4Function
optimize (argtypes,name,bs,vars,temps,strList,params) =
	let bs2 = map optGotoAftRet bs
	in let map = optToMap bs2
	in let map2 = optEraseEmptyBlocksFix map
	in (argtypes,name,optFromMap (Map.toList map2),vars,temps,strList,params)

optimizeWhole :: [Code4Function] -> [Code4Function]
optimizeWhole list = map optimize list
