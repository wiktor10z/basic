module Latc_printout where

import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import System.Environment
import System.Exit
import System.IO

import Latte.Abs
import Latte.Lex
import Latte.Par
import Latte.ErrM
import Latc_basic
import Latc_Code4


--TODO rdi....r15 są potrzebne na argumenty funkcji więc może być problem jak są aktualnie używane - sprawdzić działanie

type StAss = State (Int,Int,Int) -- trzyma off1,off2,off3


readVal :: ValVar4 -> StAss (String,String)
readVal v = do
	varloc <- getAddress v
	case varloc of
		Right str -> return (str,"")
		Left str -> case (getType v) of
			Bool -> return ("%al", "    movzbl\t "++str++", %eax\n")
			Int -> return ("%eax", "    movl\t "++str++", %eax\n")
			Str -> return ("%rax", "    movq\t "++str++", %rax\n")	--TODO sprawdzić, czy używanie tego rejestru jest bezpieczne - czy czegoś nie psuje

writeVal :: ValVar4 -> StAss (String,String)
writeVal v = do
	varloc <- getAddress v
	case varloc of
		Right str -> return (str,"")
		Left str -> case (getType v) of
			Bool -> return ("%al", "    movb\t %al, "++str++"\n")	--TODO przetestować szczególnie bool
			Int -> return ("%eax", "    movl\t %eax, "++str++"\n")
			Str -> return ("%rax", "    movq\t %rax, "++str++"\n")
			

exactAddress :: ValVar4 -> StAss String
exactAddress (Int4 n) = return ("$"++(show n))
exactAddress (Bool4 True) = return "$1"
exactAddress (Bool4 False) = return "$0"
exactAddress v = do
	varloc <- getAddress v
	case varloc of
		Right str -> return str
		Left str -> return str

getAddress :: ValVar4 -> StAss (Either String String)
getAddress (Var4 n t) = do
	varloc <- varsHash n
	return (Right ((show varloc)++"(%rbp)"))
	
getAddress (Temp4 n t) = do
	(_,_,off3) <- get
	if n<13
		then do
		varloc <- tempsHash t n
		return (Right varloc)
		else return (Left ((show ((-1)*off3))++"(%rbp)")) --TODO prawdziwy adres
--getAddress Rej4 = return (Right "%eax") --TODO czasem %eax a czasem %al - ustalić żeby było dobrze dla and i or


getType :: ValVar4 -> Type
getType (Var4 n t) = t
getType (Temp4 n t) = t
getType (Int4 _) = Int
getType (Bool4 _) = Bool
getType (String4 _) = Str
getType Rej4 = Bool




varsHash ::Int -> StAss Int	--off1 = 
varsHash n = do
	(off1,off2,_) <- get
	if n<(-100)
		then return (-n-100)
		else if n>=0
			then return (-n-off1)
			else return (n-off2)

tempsHash :: Type -> Int-> StAss String

tempsHash Str 1 =  return "%rcx"
tempsHash Str 2 =  return "%rsi"
tempsHash Str 3 =  return "%rdi"
tempsHash Str 4 =  return "%r8"
tempsHash Str 5 =  return "%r9"
tempsHash Str 6 =  return "%r10"
tempsHash Str 7 =  return "%r11"
tempsHash Str 8 =  return "%rbx"
tempsHash Str 9 =  return "%r12"
tempsHash Str 10 =  return "%r13"
tempsHash Str 11 =  return "%r14"
tempsHash Str 12 =  return "%r15"

tempsHash _ 1 =  return "%ecx"
tempsHash _ 2 =  return "%esi"
tempsHash _ 3 =  return "%edi"
tempsHash _ 4 =  return "%r8d"
tempsHash _ 5 =  return "%r9d"
tempsHash _ 6 =  return "%r10d"
tempsHash _ 7 =  return "%r11d"
tempsHash _ 8 =  return "%ebx"
tempsHash _ 9 =  return "%r12d"
tempsHash _ 10 =  return "%r13d"
tempsHash _ 11 =  return "%r14d"
tempsHash _ 12 =  return "%r15d"


pushRegisters :: Int -> String
pushRegisters 0 =""
pushRegisters 8 = "    pushq\t %rbx\n"
pushRegisters 9 =(pushRegisters 8) ++"    pushq\t %r12\n"
pushRegisters 10 =(pushRegisters 9) ++"    pushq\t %r13\n"
pushRegisters 11 =(pushRegisters 10) ++"    pushq\t %r14\n"
pushRegisters 12 =(pushRegisters 11) ++"    pushq\t %r15\n"
pushRegisters x = pushRegisters (x-1)

popRegisters :: Int -> String
popRegisters 0 =""
popRegisters 8 = "    popq\t %rbx\n"
popRegisters 9 =(pushRegisters 8) ++"    popq\t %r12\n"
popRegisters 10 =(pushRegisters 9) ++"    popq\t %r13\n"
popRegisters 11 =(pushRegisters 10) ++"    popq\t %r14\n"
popRegisters 12 =(pushRegisters 11) ++"    popq\t %r15\n"
popRegisters x = pushRegisters (x-1)



manageArgs :: [Type] -> Int -> Int -> Int -> (String,Int)
manageArgs (t:ts) 6 off off2= ((manageArgs1 t 6)++show((-1)*(valSize t)-off-off2)++"(%rbp)\n",(valSize t)+off)
manageArgs (t:ts) i off off2=
	let (str,int) = manageArgs ts (i+1) (off+(valSize t)) off2
	in ((manageArgs1 t i)++show((-1)*(valSize t)-off-off2)++"(%rbp)\n"++str,int)
manageArgs [] _ off off2= ("",off)

manageArgs1 :: Type -> Int -> String

manageArgs1 Int 1 = "    movl\t %edi, "
manageArgs1 Int 2 = "    movl\t %esi, "
manageArgs1 Int 3 = "    movl\t %edx, "
manageArgs1 Int 4 = "    movl\t %ecx, "
manageArgs1 Int n = "    movl\t %r"++show(n+3)++"d, "

manageArgs1 Str 1 = "    movq\t %rdi, "
manageArgs1 Str 2 = "    movq\t %rsi, "
manageArgs1 Str 3 = "    movq\t %rdx, "
manageArgs1 Str 4 = "    movq\t %rcx, "
manageArgs1 Str n = "    movq\t %r"++show(n+3)++", "

manageArgs1 Bool 1 = "    movl\t %edi, %eax\n    movb\t %al, "
manageArgs1 Bool 2 = "    movl\t %esi, %eax\n    movb\t %al, "
manageArgs1 Bool 3 = "    movl\t %edx, %eax\n    movb\t %al, "
manageArgs1 Bool 4 = "    movl\t %ecx, %eax\n    movb\t %al, "
manageArgs1 Bool n = "    movl\t %r"++show(n+3)++"d, %eax\n    movb\t %al, "


functionIntro :: [Type] -> Int -> Int -> (String,Int,Int,Int)
functionIntro args vars temps = 
	let (str,off2) = manageArgs args 1 (((div (vars+15) 16))*16) ((max 0 (min (temps-7) 5))*8)
	in ((pushRegisters temps) ++
	"    subq\t $"++(show ((((div (off2+7) 8))*8)+((max (temps - 12) 0)*8)))++", %rsp\n"++str
	,((max 0 (min (temps-7) 5))*8),(((div (vars+15) 16))*16)+((max 0 (min (temps-7) 5))*8),(((div (off2+7) 8))*8)+((max 0 (min (temps-7) 5))*8)+8)

--(((div (vars+15) 16))*16) - zaokrąglenie miejsca zajmowanego przez zmienne w górę do podzielnego przez 16
--((max 0 (min (temps-7) 5))*8) - (tempy minus liczba wolnych rejestrów) - ile z rejestrów które muszą być zachowane będzie używane
--((max (temps - 12) 0)*8))) - miejsce zaarezerwowane na dodatkowe tempy
--(((div (off2+7) 8))*8) - wysokość stosu do przesunięcia wyliczona po wpisaniu argumetnów funkcji z rejestrów na stos

functionOutro :: Int -> String
functionOutro temps = (popRegisters temps)++"    leave\n    ret\n"

movString ::ValVar4 -> ValVar4 -> StAss String
movString (Var4 var1 t1) (Var4 var2 t2) = 
	if (var1==var2)
		then return ""
		else do
			(varloc1,_) <- writeVal (Var4 var1 t1)
			(varloc2,_) <- readVal (Var4 var2 t2)
			case t1 of
				Bool -> return ("    movzbl\t "++varloc2++", %eax\n    "++"movb\t %al, "++varloc1++"\n")
				Int -> return ("    movl\t "++varloc2++", %eax\n    movl\t %eax, "++varloc1++"\n")
				Str -> return ("    movq\t "++varloc2++", %rax\n    movq\t %rax, "++varloc1++"\n")

movString var1 (Int4 n) = do
	varloc1 <- exactAddress var1
	return ("    movl\t $"++(show n)++", "++varloc1++"\n")

movString Rej4 var1 = do
	varloc1 <- exactAddress var1
	return ("    movl\t "++varloc1++", %eax\n")

movString (Temp4 x y) Rej4 = do
	varloc1 <- getAddress (Temp4 x y)
	case varloc1 of
		Left loc -> return ("    movb\t %al, "++loc++"\n")
		Right loc -> return ("    movzbl\t %al, "++loc++"\n")

movString var1 Rej4 = do
	varloc1 <- exactAddress var1
	return ("    movb\t %al, "++varloc1++"\n")

movString var1 (Bool4 True) = do
	varloc1 <- exactAddress var1
	return ("    movb\t $1, "++varloc1++"\n")

movString var1 (Bool4 False) = do
	varloc1 <- exactAddress var1
	return ("    movb\t $0, "++varloc1++"\n")

movString var1 var2 =
	if (var1==var2)
		then return ""
		else do
			(varloc1,str1) <- writeVal var1
			(varloc2,str2) <- readVal var2
			case (getType var1) of
				Bool -> return (str2++"    movzbl\t "++varloc2++", %eax\n    "++"movb\t %al, "++varloc1++"\n"++str1)
				Int -> return (str2++"    movl\t "++varloc2++", "++varloc1++"\n"++str1)
				Str -> return (str2++"    movq\t "++varloc2++", "++varloc1++"\n"++str1)

movAL :: ValVar4 -> StAss String
movAL (Temp4 x y)  = do
	varloc1 <- getAddress (Temp4 x y)
	case varloc1 of
		Left loc -> return ("    movb\t %al, "++loc++"\n")
		Right loc -> return ("    movzbl\t %al, "++loc++"\n")
		
movAL var1 = do
	varloc1 <- exactAddress var1
	return ("    movb\t %al, "++varloc1++"\n")

operString :: String -> ValVar4 -> ValVar4 -> StAss String
operString oper (Var4 var1 t1) (Var4 var2 t2) = do
	(varloc1,_) <- writeVal (Var4 var1 t1)
	(varloc2,_) <- readVal (Var4 var2 t2)
	case t1 of
		Str -> return ("    movq\t "++varloc2++", %rax\n    "++oper++"\t %rax, "++varloc1++"\n")
		_ ->  return ("    movl\t "++varloc2++", %eax\n    "++oper++"\t %eax, "++varloc1++"\n")
		
operString oper var1 var2 = do
	(varloc1,str1) <- writeVal var1
	(varloc2,str2) <- readVal var2
	return (str2++"    "++oper++"\t "++varloc2++", "++varloc1++"\n"++str1)




assembleInstruction :: Code4Instruction -> StAss String
--TODO przypisanie stringa

assembleInstruction (Ass4 var1 var2)= movString var1 var2


assembleInstruction (Param4 1 var) = movString (Temp4 3 (getType var)) var
assembleInstruction (Param4 2 var) = movString (Temp4 2 (getType var)) var

assembleInstruction (Param4 3 var) =
	if ((getType var)==Str)
		then do
			varloc <- exactAddress var
			return ("    movq\t "++varloc++", %rdx\n")
		else do
			varloc <- exactAddress var
			return ("    movq\t "++varloc++", %edx\n")
			
assembleInstruction (Param4 4 var) = movString (Temp4 1 (getType var)) var
assembleInstruction (Param4 5 var) = movString (Temp4 4 (getType var)) var	
assembleInstruction (Param4 6 var) = movString (Temp4 5 (getType var)) var
	
assembleInstruction (Param4 n var) = return "TODO zrobić push na stos - i jakieś wyrównanie\n" 
	
assembleInstruction (CallV var name _ ) = do
	varloc <- exactAddress var
	case (getType var) of
		Bool -> return ("    call\t "++name++"\n    movb\t %al, "++varloc++"\n")
		Int -> return ("    call\t "++name++"\n    movl\t %eax, "++varloc++"\n")
		Str -> return ("    call\t "++name++"\n    movq\t %rax, "++varloc++"\n")
		Void -> return("    call\t"++name++"\n")


assembleInstruction (Return4 Void4) = return ("")
assembleInstruction (Return4 var) = do
	varloc <- exactAddress var
	if ((getType var)==Str)
		then return ("    movq\t"++varloc++", %rax\n")
		else return ("    movl\t"++varloc++", %eax\n")
		--TODO może jeszcze co innego dla bool


assembleInstruction (If4 var str) = do
	varloc <- exactAddress var
	return ("    cmpb $0, "++varloc++"\n    jne ."++str++"\n")
assembleInstruction (Goto4 str) = return ("    jmp ."++str++"\n")





assembleInstruction (OpV Add4 var1 var2 (Int4 n))= do
	str <- movString var1 var2
	varloc <- exactAddress var1
	return (str ++"    addl\t $"++(show n)++", "++varloc++"\n")
		
assembleInstruction (OpV Add4 var1 (Int4 n) var2)= do
	str <- movString var1 var2
	varloc <- exactAddress var1	
	return (str ++"    addl\t $"++(show n)++", "++varloc++"\n")

assembleInstruction (OpV Add4 var1 var2 var3)=
	if (var1==var2)
		then do
			operString "addl" var1 var3
		else if (var1==var3)
			then do
				operString "addl" var1 var2
			else do
				str <- movString var1 var2
				str2 <- operString "addl" var1 var3
				return (str++str2)

assembleInstruction (OpV Sub4 var1 var2 (Int4 n)) = do
	str <- movString var1 var2
	varloc <- exactAddress var1
	return (str ++"    subl\t $"++(show n)++", "++varloc++"\n")
	
assembleInstruction (OpV Sub4 var1 (Int4 n) var2)= do
	str <- movString var1 var2
	varloc <- exactAddress var1
	return (str ++"    subl\t $"++(show n)++", "++varloc++"\n")

assembleInstruction (OpV Sub4 var1 var2 var3)=
	if (var1==var2)
		then do
			operString "subl" var1 var3
		else if (var1==var3)
			then do
				operString "subl" var1 var2
			else do
				str <- movString var1 var2
				str2 <- operString "subl" var1 var3
				return (str++str2)

		
assembleInstruction (OpV Mul4 var1 (Int4 n) var2) = do	--TODO można by zrobić tak jak w c - binarne dodawanie i przesuwanie
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	return ("    movl\t "++loc2++", %eax\n    imull\t $"++(show n)++", %eax\n    movl\t %eax, "++loc1++"\n")

assembleInstruction (OpV Mul4 var1 var2 var3) =	do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    imull\t "++loc3++", %eax\n    movl\t %eax, "++loc1++"\n")


assembleInstruction (OpV Div4 var1 var2 var3) = do	--TODO można by zrobić tak jak w c - binarne dodawanie i przesuwanie
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cltd\n    idivl\t "++loc3++"\n    movl\t %eax, "++loc1++"\n")

assembleInstruction (OpV Mod4 var1 var2 var3) = do	--TODO można by zrobić tak jak w c - binarne dodawanie i przesuwanie przy stałych
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cltd\n    idivl\t "++loc3++"\n    movl\t %edx, "++loc1++"\n")

--TODO może być porównanie miedzy rejestrem i czymś - wtedy nie trzeba przepisywać
assembleInstruction (OpV SetL4 var1 var2 var3) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setl\t %al\n    movb\t %al, "++loc1++"\n")

assembleInstruction (OpV SetLE4 var1 var2 var3) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setle\t %al\n    movb\t %al, "++loc1++"\n")

assembleInstruction (OpV SetG4 var1 var2 var3) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setg\t %al\n    movb\t %al, "++loc1++"\n")

assembleInstruction (OpV SetGE4 var1 var2 var3) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setge\t %al\n    movb\t %al, "++loc1++"\n")

assembleInstruction (OpV SetE4B var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movzbl\t "++loc2++", %eax\n    testl\t "++loc3++", %eax\n    setne\t %al\n"++str1) -- jeżeli lokacja jest rejestrem, to movzbl

assembleInstruction (OpV SetE4I var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    sete\t %al\n"++str1)
	
assembleInstruction (OpV SetE4S var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movq\t "++loc2++", %eax\n    cmpq\t "++loc3++", %eax\n    sete\t %al\n"++str1)


--TODOTODO równośc i nierówność, if



assembleInstruction _ = return "TODO\n"


assembleInstrs :: [Code4Instruction] -> StAss String
assembleInstrs (inst:instrs) = do
	str1 <- assembleInstruction inst
	str2 <- assembleInstrs instrs
	return (str1++str2)
	
assembleInstrs [] = return ""
	
assembleBlock :: Code4Block -> StAss String --TODO to blok zerowy powinien zawierać push\q...
assembleBlock (label,instrs) = do
	str <- assembleInstrs instrs
	return ("."++label++":\n"++str)




assembleFunCode :: [Code4Block] -> StAss (String)
assembleFunCode (b:bs) = do
	str1 <-assembleBlock b
	str2 <-assembleFunCode bs
	return (str1++str2)

assembleFunCode [] = return ""


assembleTopDef :: Code4Function -> IO()--tutaj wypisać prolog i epilog funkcji
assembleTopDef (argtypes,name,((label,inst):bs),vars,temps)  = do
	let (intro,off1,off2,off3) =functionIntro argtypes vars temps
	putStrLn ("    .globl  "++name++"\n"++name++":\n"++"."++label++":\n    pushq\t %rbp\n    movq\t %rsp, %rbp\n"++intro)		--TODO Code4Function mus i zawierać jeszcze listę typów argumentów
	let (str1,_) = runState (assembleInstrs inst) (off1,off2,off3)	--TODOTODO prawdziwe offsety wyciagnięte z functionIntro
	let (str2,_) = runState (assembleFunCode (init bs)) (off1,off2,off3)
	putStr (str1++str2)
	putStr ("."++name++"_END:\n"++(functionOutro temps))


assembleWhole :: [Code4Function] -> IO ()
assembleWhole (f:fs) = do
	assembleTopDef f
	assembleWhole fs

assembleWhole [] = return ()


