module Latc_printout where

import Control.Monad.Reader
import Control.Monad.State
import System.Environment
import System.IO

import Latte.Abs
import Latc_basic
import Latc_Code4

--stan nie zmienia się, zawiera nazwę funkcji i informację o przesunięciach pozwalające odnaleźć lokację na stosie
type StAss = State (String,Int,Int,Int)
		
--adres w pamięci jako string
exactAddress :: ValVar4 -> StAss String
exactAddress (Int4 n) = return ("$"++(show n))
exactAddress (Bool4 True) = return "$1"
exactAddress (Bool4 False) = return "$0"
exactAddress (String4 n) = do
	(name,_,_,_) <- get
	return ("$."++(ciapkasReplace name)++"_string"++(show n))
exactAddress v = do
	varloc <- getAddress v
	case varloc of
		Right str -> return str
		Left str -> return str

--adres w pamięci - left -> stos, right -> rejestr lub wartość stała
getAddress :: ValVar4 -> StAss (Either String String)
getAddress (Var4 n t) = do
	varloc <- varsHash n
	return (Left ((show varloc)++"(%rbp)"))
	
getAddress (Temp4 n t) = do
	(_,_,_,off3) <- get
	if n<13
		then do
		varloc <- tempsHash t n
		return (Right varloc)
		else return (Left ((show ((-1)*off3))++"(%rbp)"))
		
getAddress (Int4 n) =  return (Right ("$"++(show n)))
getAddress (Bool4 True) = return (Right "$1")
getAddress (Bool4 False) = return (Right "$0")
getAddress (Pointer4 n) = return (Right ("$"++(show n)))
	
getAddress (String4 n) = do
	(name,_,_,_) <- get
	return (Right ("$."++(ciapkasReplace name)++"_string"++(show n)))

getType :: ValVar4 -> Type
getType (Var4 n t) = t
getType (Temp4 n t) = t
getType (Int4 _) = Int
getType (Bool4 _) = Bool
getType (String4 _) = Str
getType (ArrElem4 _ t _) = t
getType (ObjAttr4 _ t _) = t
getType (Pointer4 _) = Str	--to jest tak na prawdę null o braku typu
getType Rej4 = Bool

--przemapowanie zmiennych z kodu czwórkowego na adresy i rejestry

varsHash ::Int -> StAss Int 
varsHash n = do
	(_,off1,off2,_) <- get
	if n<(-100)
		then return (-n-100)
		else if n>=0
			then return (-n-off1)
			else return (n-off2)

tempsHash :: Type -> Int-> StAss String

tempsHash Bool 1 =  return "%ecx"
tempsHash Bool 2 =  return "%esi"
tempsHash Bool 3 =  return "%edi"
tempsHash Bool 4 =  return "%r8d"
tempsHash Bool 5 =  return "%r9d"
tempsHash Bool 6 =  return "%r10d"
tempsHash Bool 7 =  return "%r11d"
tempsHash Bool 8 =  return "%ebx"
tempsHash Bool 9 =  return "%r12d"
tempsHash Bool 10 =  return "%r13d"
tempsHash Bool 11 =  return "%r14d"
tempsHash Bool 12 =  return "%r15d"

tempsHash Int 1 =  return "%ecx"
tempsHash Int 2 =  return "%esi"
tempsHash Int 3 =  return "%edi"
tempsHash Int 4 =  return "%r8d"
tempsHash Int 5 =  return "%r9d"
tempsHash Int 6 =  return "%r10d"
tempsHash Int 7 =  return "%r11d"
tempsHash Int 8 =  return "%ebx"
tempsHash Int 9 =  return "%r12d"
tempsHash Int 10 =  return "%r13d"
tempsHash Int 11 =  return "%r14d"
tempsHash Int 12 =  return "%r15d"

tempsHash _ 1 =  return "%rcx"
tempsHash _ 2 =  return "%rsi"
tempsHash _ 3 =  return "%rdi"
tempsHash _ 4 =  return "%r8"
tempsHash _ 5 =  return "%r9"
tempsHash _ 6 =  return "%r10"
tempsHash _ 7 =  return "%r11"
tempsHash _ 8 =  return "%rbx"
tempsHash _ 9 =  return "%r12"
tempsHash _ 10 =  return "%r13"
tempsHash _ 11 =  return "%r14"
tempsHash _ 12 =  return "%r15"


pushRegisters :: Int -> String
pushRegisters 0 =""
pushRegisters 8 = "    pushq\t %rbx\n"
pushRegisters 9 ="    pushq\t %r12\n"++(pushRegisters 8)
pushRegisters 10 ="    pushq\t %r13\n"++(pushRegisters 9)
pushRegisters 11 ="    pushq\t %r14\n"++(pushRegisters 10)
pushRegisters 12 ="    pushq\t %r15\n"++(pushRegisters 11)
pushRegisters x = pushRegisters (x-1)

popRegisters :: Int -> String
popRegisters 0 =""
popRegisters 8 = "    popq\t %rbx\n"
popRegisters 9 =(popRegisters 8) ++"    popq\t %r12\n"
popRegisters 10 =(popRegisters 9) ++"    popq\t %r13\n"
popRegisters 11 =(popRegisters 10) ++"    popq\t %r14\n"
popRegisters 12 =(popRegisters 11) ++"    popq\t %r15\n"
popRegisters x = popRegisters (x-1)



manageArgs :: [Type] -> Int -> Int -> Int -> (String,Int)
manageArgs (t:ts) 6 off off2= ((manageArgs1 t 6)++show((-1)*(valSize t)-off-off2)++"(%rbp)\n",(valSize t)+off)
manageArgs (t:ts) i off off2=
	let (str,int) = manageArgs ts (i+1) (off+(valSize t)) off2
	in ((manageArgs1 t i)++show((-1)*(valSize t)-off-off2)++"(%rbp)\n"++str,int)
manageArgs [] _ off off2= ("",off)

manageArgs1 :: Type -> Int -> String

manageArgs1 Bool 1 = "    movl\t %edi, %eax\n    movb\t %al, "
manageArgs1 Bool 2 = "    movl\t %esi, %eax\n    movb\t %al, "
manageArgs1 Bool 3 = "    movl\t %edx, %eax\n    movb\t %al, "
manageArgs1 Bool 4 = "    movl\t %ecx, %eax\n    movb\t %al, "
manageArgs1 Bool n = "    movl\t %r"++show(n+3)++"d, %eax\n    movb\t %al, "

manageArgs1 Int 1 = "    movl\t %edi, "
manageArgs1 Int 2 = "    movl\t %esi, "
manageArgs1 Int 3 = "    movl\t %edx, "
manageArgs1 Int 4 = "    movl\t %ecx, "
manageArgs1 Int n = "    movl\t %r"++show(n+3)++"d, "

manageArgs1 _ 1 = "    movq\t %rdi, "
manageArgs1 _ 2 = "    movq\t %rsi, "
manageArgs1 _ 3 = "    movq\t %rdx, "
manageArgs1 _ 4 = "    movq\t %rcx, "
manageArgs1 _ n = "    movq\t %r"++show(n+3)++", "


functionStrings :: String ->[(String,Int)] -> String
functionStrings name ((str,n):ls) = ("."++name++"_string"++(show n)++":\n    .string "++show(str)++"\n"++(functionStrings name ls))
functionStrings name [] = []


--funkcja tworząca string wejściowy funkcji - operacje na argumentach funkcji, wyliczanie przesunięć zawartych w stanie, przesuwanie rsp
functionIntro :: [Type] -> Int -> Int -> Int -> (String,Int,Int,Int,Int)
functionIntro args vars temps params = 
	let (str,off2) = manageArgs args 1 (((div (vars+15) 16))*16) ((max 0 (min (temps-7) 5))*8)
	in ((pushRegisters temps) ++
	"    subq\t $"++(show ((((div (off2+7) 8))*8)+((max (temps - 12) 0)*8)+((max 0 (params -6 ))*8)))++", %rsp\n"++str,
	((max 0 (min (temps-7) 5))*8),(((div (vars+15) 16))*16)+((max 0 (min (temps-7) 5))*8),
	(((div (off2+7) 8))*8)+((max 0 (min (temps-7) 5))*8)+8,(((div (off2+7) 8))*8)+((max (temps - 12) 0)*8)+((max 0 (params -6 ))*8))

--(((div (vars+15) 16))*16) - zaokrąglenie miejsca zajmowanego przez zmienne w górę do podzielnego przez 16
--((max 0 (min (temps-7) 5))*8) - (tempy minus liczba wolnych rejestrów) - ile z rejestrów które muszą być zachowane będzie używane
--((max 0 (params -6 ))*8) - miejsce zarezerwowane na parametry wołania funkcji przekazywane na stosie (7+)
--((max (temps - 12) 0)*8))) - miejsce zaarezerwowane na dodatkowe tempy
--(((div (off2+7) 8))*8) - wysokość stosu do przesunięcia wyliczona po wpisaniu argumetnów funkcji z rejestrów na stos

functionOutro :: Int -> Int -> String
functionOutro move temps = "    addq\t $"++show(move)++", %rsp\n" ++(popRegisters temps)++"    leave\n    ret\n"

--funkcja obsługująca przenoszenie wartości między różnymi typami zmiennych i stałych
movString ::ValVar4 -> ValVar4 -> StAss String
movString (Var4 var1 t1) (Var4 var2 t2) = 
	if (var1==var2)
		then return ""
		else do
			varloc1 <- exactAddress (Var4 var1 t1)
			varloc2 <- exactAddress (Var4 var2 t2)
			case t1 of
				Bool -> return ("    movzbl\t "++varloc2++", %eax\n    "++"movb\t %al, "++varloc1++"\n")
				Int -> return ("    movl\t "++varloc2++", %eax\n    movl\t %eax, "++varloc1++"\n")
				_ -> return ("    movq\t "++varloc2++", %rax\n    movq\t %rax, "++varloc1++"\n")

movString (ObjAttr4 obj1 t off1) (ObjAttr4 obj2 _ off2) = do
	objloc1 <- exactAddress obj1
	objloc2 <- exactAddress obj2
	let str1 = "    movq\t "++objloc1++", %rdx\n    addq\t $"++(show off1)++", %rdx\n"
	let str2 = "    movq\t "++objloc2++", %rax\n    addq\t $"++(show off2)++", %rax\n"
	case t of
		Bool -> return (str1++str2++"    movzbl\t (%rax), %eax\n    movb\t %al, (%rdx)\n")
		Int -> return (str1++str2++"    movl\t (%rax), %eax\n    movl\t %eax, (%rdx)\n")
		_ -> return (str1++str2++"    movq\t (%rax), %rax\n    movq\t %rax, (%rdx)\n")

movString (ArrElem4 arr t var1) (ObjAttr4 obj2 _ off2) = do
	arrloc <- exactAddress arr
	objloc2 <- exactAddress obj2
	loc1 <- exactAddress var1
	let str1 = "    movl\t "++loc1++", %edx\n    addq\t "++arrloc++", %rdx\n"
	let str2 = "    movq\t "++objloc2++", %rax\n    addq\t $"++(show off2)++", %rax\n"
	case t of
		Bool -> return (str1++str2++"    movzbl\t (%rax), %eax\n    movb\t %al, (%rdx)\n")
		Int -> return (str1++str2++"    movl\t (%rax), %eax\n    movl\t %eax, (%rdx)\n")
		_ -> return (str1++str2++"    movq\t (%rax), %rax\n    movq\t %rax, (%rdx)\n")

movString var1 (ObjAttr4 obj t off) = do
	objloc <- exactAddress obj
	varloc1 <- getAddress var1
	let str = "    movq\t "++objloc++", %rax\n    addq\t $"++(show off)++", %rax\n"
	case varloc1 of
		Left loc1 -> case t of
			Bool -> return (str++"    movzbl\t (%rax), %eax\n    movb\t %al, "++loc1++"\n")
			Int -> return (str++"    movl\t (%rax), %eax\n    movl\t %eax, "++loc1++"\n")
			_ -> return (str++"    movq\t (%rax), %rax\n    movq\t %rax, "++loc1++"\n")
		Right loc1 -> case t of
			Bool -> return (str++"    movzbl\t (%rax), "++loc1++"\n")--TODO to oczywiście trzeba przetestować i poprawić, bo prawie na pewno źle
			Int -> return (str++"    movl\t (%rax), "++loc1++"\n")
			_ -> return (str++"    movq\t (%rax), "++loc1++"\n")

movString (ObjAttr4 obj t off) var1 = do
	objloc <- exactAddress obj
	varloc1 <- getAddress var1
	loc1 <- exactAddress var1
	let str = "    movq\t "++objloc++", %rax\n    addq\t $"++(show off)++", %rax\n"
	case t of
		Bool -> case var1 of
			(Bool4 _) -> return (str++"    movb\t "++loc1++", (%rax)\n")
			_ -> return (str++"    movl\t "++loc1++", %edx\n    movb\t %dl, (%rax)\n")
		Int-> case varloc1 of
			Left _ -> return (str++"    movl\t "++loc1++", %edx\n    movl\t %edx, (%rax)\n")
			Right _ -> return (str++"    movl\t "++loc1++", (%rax)\n")
		_ -> case varloc1 of
			Left _ -> return (str++"    movq\t "++loc1++", %rdx\n    movq\t %rdx, (%rax)\n")
			Right _ -> return (str++"    movq\t "++loc1++", (%rax)\n")
			
movString var1 (ArrElem4 arr t var2) = do
	arrloc <- exactAddress arr
	varloc1 <- getAddress var1
	loc2 <- exactAddress var2
	let str = "    movl\t "++loc2++", %eax\n    addq\t "++arrloc++", %rax\n"
	case varloc1 of
		Left loc1 -> case t of
			Bool -> return (str++"    movzbl\t (%rax), %eax\n    movb\t %al, "++loc1++"\n")
			Int -> return (str++"    movl\t (%rax), %eax\n    movl\t %eax, "++loc1++"\n")
			_ -> return (str++"    movq\t (%rax), %rax\n    movq\t %rax, "++loc1++"\n")
		Right loc1 -> case t of
			Bool -> return (str++"    movzbl\t (%rax), "++loc1++"\n")--TODO to oczywiście trzeba przetestować i poprawić, bo prawie na pewno źle
			Int -> return (str++"    movl\t (%rax), "++loc1++"\n")
			_ -> return (str++"    movq\t (%rax), "++loc1++"\n")

movString (ArrElem4 arr t var2) var1 = do
	arrloc <- exactAddress arr
	varloc1 <- getAddress var1
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	let str = "    movl\t "++loc2++", %eax\n    addq\t "++arrloc++", %rax\n"
	case t of
		Bool -> case var1 of
			(Bool4 _) -> return (str++"    movb\t "++loc1++", (%rax)\n")
			_ -> return (str++"    movl\t "++loc1++", %edx\n    movb\t %dl, (%rax)\n")
		Int-> case varloc1 of
			Left _ -> return (str++"    movl\t "++loc1++", %edx\n    movl\t %edx, (%rax)\n")
			Right _ -> return (str++"    movl\t "++loc1++", (%rax)\n")
		_ -> case varloc1 of
			Left _ -> return (str++"    movq\t "++loc1++", %rdx\n    movq\t %rdx, (%rax)\n")
			Right _ -> return (str++"    movq\t "++loc1++", (%rax)\n")

movString var1 (Int4 n) = do
	varloc1 <- exactAddress var1
	return ("    movl\t $"++(show n)++", "++varloc1++"\n")

movString Rej4 var1 = do
	varloc1 <- exactAddress var1
	return ("    movl\t "++varloc1++", %eax\n")

movString var1 Rej4 = do
	varloc1 <- getAddress var1
	case varloc1 of
		Left loc -> return ("    movb\t %al, "++loc++"\n")
		Right loc -> return ("    movzbl\t %al, "++loc++"\n")

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
			varloc1 <- getAddress var1
			varloc2 <- getAddress var2
			loc1 <- exactAddress var1
			loc2 <- exactAddress var2
			case (getType var1) of
				Bool -> do
					str3 <- movAL var1
					case varloc2 of
						Right _ -> return ("    movl\t "++loc2++", %eax\n"++str3)
						Left _ ->return ("    movzbl\t "++loc2++", %eax\n"++str3)
				Int -> do
					case (varloc1,varloc2) of
						(Left _,Left _) -> return ("    movl\t "++loc2++", %eax\n    movl\t %eax, "++loc1++"\n")
						_ -> return ("    movl\t "++loc2++", "++loc1++"\n")
				_ -> do
					case (varloc1,varloc2) of
						(Left _,Left _) -> return ("    movq\t "++loc2++", %rax\n    movq\t %rax, "++loc1++"\n")
						_ -> return ("    movq\t "++loc2++", "++loc1++"\n")

movAL :: ValVar4 -> StAss String
movAL var1  = do
	varloc1 <- getAddress var1
	case varloc1 of
		Left loc -> return ("    movb\t %al, "++loc++"\n")
		Right loc -> return ("    movzbl\t %al, "++loc++"\n")

operString :: String -> ValVar4 -> ValVar4 -> StAss String
operString oper var1 var2 = do
	varloc1 <- getAddress var1
	varloc2 <- getAddress var2
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	case (getType var1) of
		Bool -> do
			str3 <- movAL var1
			case varloc2 of
				Right _ -> return ("    "++oper++"\t "++loc2++", %eax\n"++str3)
				Left _ ->return ("    "++oper++"\t "++loc2++", %eax\n"++str3)
		Int -> do
			case (varloc1,varloc2) of
				(Left _,Left _) -> return ("    movl\t "++loc2++", %eax\n    "++oper++"\t %eax, "++loc1++"\n")
				_ -> return ("    "++oper++"\t "++loc2++", "++loc1++"\n")
		_ -> do
			case (varloc1,varloc2) of
				(Left _,Left _) -> return ("    movq\t "++loc2++", %rax\n    "++oper++"\t %rax, "++loc1++"\n")
				_ -> return ("    "++oper++"\t "++loc2++", "++loc1++"\n")


---------------------------------------------------------------------------------------------------------------

assembleInstruction :: Code4Instruction -> StAss String

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
			return ("    movl\t "++varloc++", %edx\n")
			
assembleInstruction (Param4 4 var) = movString (Temp4 1 (getType var)) var
assembleInstruction (Param4 5 var) = movString (Temp4 4 (getType var)) var	
assembleInstruction (Param4 6 var) = movString (Temp4 5 (getType var)) var
	
assembleInstruction (Param4 n var) = do
	varloc <- getAddress var
	case varloc of
		Right loc -> case (getType var) of
			Bool -> return ("    movl\t "++loc++", "++(show ((n-7)*8))++"(%rsp)\n")
			Int -> return ("    movl\t "++loc++", "++(show ((n-7)*8))++"(%rsp)\n")
			_ -> return ("    movq\t "++loc++", "++(show ((n-7)*8))++"(%rsp)\n")
		Left loc -> case (getType var) of
			Bool -> return ("    movzbl\t "++loc++", "++(show ((n-7)*8))++"(%rsp)\n")
			Int -> return ("    movl\t "++loc++", "++(show ((n-7)*8))++"(%rsp)\n")
			_ -> return ("    movq\t "++loc++", "++(show ((n-7)*8))++"(%rsp)\n")
	
assembleInstruction (CallV var name _ ) = do
	varloc <- exactAddress var
	case (getType var) of
		Bool -> do
			str <- movAL var
			return ("    call\t "++(ciapkasReplace name)++"\n"++str)
		Int -> return ("    call\t "++(ciapkasReplace name)++"\n    movl\t %eax, "++varloc++"\n")
		Void -> return("    call\t "++(ciapkasReplace name)++"\n")		
		_ -> return ("    call\t "++(ciapkasReplace name)++"\n    movq\t %rax, "++varloc++"\n")

assembleInstruction (Return4 Void4) = return ("")
assembleInstruction (Return4 var) = do
	varloc <- exactAddress var
	if (((getType var)==Int )||((getType var)==Bool))
		then return ("    movl\t"++varloc++", %eax\n")	
		else return ("    movq\t"++varloc++", %rax\n")

assembleInstruction (If4 var str) = do
	varloc <- getAddress var
	case varloc of
		Left loc -> return ("    cmpb\t $0, "++loc++"\n    jne ."++(ciapkasReplace str)++"\n")
		Right loc -> return ("    movl\t "++loc++", %eax\n    testb\t %al, %al\n    jne\t ."++(ciapkasReplace str)++"\n")
assembleInstruction (Goto4 str) = return ("    jmp ."++(ciapkasReplace str)++"\n")

assembleInstruction (Not4 var) = do
	varloc <- getAddress var
	case varloc of
		Left loc -> return ("movzbl\t "++loc++", %eax\n xorl\t $1, %eax\n movzbl\t %al, "++loc++"\n")
		Right loc -> return ("xorl\t $1, "++loc++"\n")
	
assembleInstruction (Neg4 var) = do
	varloc <- exactAddress var
	return ("    negl\t "++varloc++"\n")

assembleInstruction (AllocArr14 var1 var2 var3) = do
	varloc1 <- exactAddress var1
	varloc2 <- exactAddress var2
	varloc3 <- exactAddress var3
	return ("    movl\t "++varloc2++", %edi\n    movl\t "++varloc3++", %esi\n    call\t allocatearr\n    movq\t %rax, "++varloc1++"\n")

assembleInstruction (AllocArr24 var1 var2 var3) = do
	varloc1 <- exactAddress var1
	varloc2 <- exactAddress var2
	varloc3 <- exactAddress var3
	return ("    movl\t "++varloc2++", %edi\n    movq\t "++varloc3++", %rsi\n    call\t allocatearrofpointers\n    movq\t %rax, "++varloc1++"\n")

assembleInstruction (AllocObj4 var size) = do
	varloc <- exactAddress var
	return ("    movl\t $"++show(size)++", %edi\n    call\t alloc\n    movq\t %rax, "++varloc++"\n")

assembleInstruction (OpV Concat4 var1 var2 var3)= do
	varloc <- exactAddress var1
	str2 <- movString (Temp4 3 (getType var2)) var2
	str3 <- movString (Temp4 2 (getType var3)) var3
	return (str2++str3++"    call\t concat\n    movq\t %rax, "++varloc++"\n")

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

assembleInstruction (OpV Add24 var1 var2 (Int4 n))= do
	str <- movString var1 var2
	varloc <- exactAddress var1
	return (str ++"    addq\t $"++(show n)++", "++varloc++"\n")
		
assembleInstruction (OpV Add24 var1 (Int4 n) var2)= do
	str <- movString var1 var2
	varloc <- exactAddress var1	
	return (str ++"    addq\t $"++(show n)++", "++varloc++"\n")

assembleInstruction (OpV Add24 var1 var2 var3)=
	if (var1==var2)
		then do
			operString "addq" var1 var3
		else if (var1==var3)
			then do
				operString "addq" var1 var2
			else do
				str <- movString var1 var2
				str2 <- operString "addq" var1 var3
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
				str <- operString "subl" var1 var2
				loc <-exactAddress var1
				return (str++"    negl\t "++loc++"\n")
			else do
				str <- movString var1 var2
				str2 <- operString "subl" var1 var3
				return (str++str2)

assembleInstruction (OpV Mul4 var1 (Int4 n) var2) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	return ("    movl\t "++loc2++", %eax\n    imull\t $"++(show n)++", %eax\n    movl\t %eax, "++loc1++"\n")

assembleInstruction (OpV Mul4 var1 var2 var3) =	do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    imull\t "++loc3++", %eax\n    movl\t %eax, "++loc1++"\n")


assembleInstruction (OpV Div4 var1 var2 var3) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cltd\n    idivl\t "++loc3++"\n    movl\t %eax, "++loc1++"\n")

assembleInstruction (OpV Mod4 var1 var2 var3) = do
	loc1 <- exactAddress var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cltd\n    idivl\t "++loc3++"\n    movl\t %edx, "++loc1++"\n")

assembleInstruction (OpV SetL4 var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setl\t %al\n"++str1)

assembleInstruction (OpV SetLE4 var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setle\t %al\n"++str1)

assembleInstruction (OpV SetG4 var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setg\t %al\n"++str1)

assembleInstruction (OpV SetGE4 var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setge\t %al\n"++str1)

assembleInstruction (OpV SetE4B var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movzbl\t "++loc2++", %eax\n    testl\t "++loc3++", %eax\n    setne\t %al\n"++str1)

assembleInstruction (OpV SetE4I var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    sete\t %al\n"++str1)
	
assembleInstruction (OpV SetE4S var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movq\t "++loc2++", %rax\n    cmpq\t "++loc3++", %rax\n    sete\t %al\n"++str1)
	
assembleInstruction (OpV SetNE4B var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movzbl\t "++loc2++", %eax\n    testl\t "++loc3++", %eax\n    sete\t %al\n"++str1)

assembleInstruction (OpV SetNE4I var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movl\t "++loc2++", %eax\n    cmpl\t "++loc3++", %eax\n    setne\t %al\n"++str1)
	
assembleInstruction (OpV SetNE4S var1 var2 var3) = do
	str1 <- movAL var1
	loc2 <- exactAddress var2
	loc3 <- exactAddress var3
	return ("    movq\t "++loc2++", %rax\n    cmpq\t "++loc3++", %rax\n    setne\t %al\n"++str1)

assembleInstrs :: [Code4Instruction] -> StAss String
assembleInstrs (inst:instrs) = do
	str1 <- assembleInstruction inst
	str2 <- assembleInstrs instrs
	return (str1++str2)
	
assembleInstrs [] = return ""


assembleBlock :: Code4Block -> StAss String
assembleBlock (label,instrs) = do
	str <- assembleInstrs instrs
	return ("."++(ciapkasReplace label)++":\n"++str)


assembleFunCode :: [Code4Block] -> StAss (String)
assembleFunCode (b:bs) = do
	str1 <-assembleBlock b
	str2 <-assembleFunCode bs
	return (str1++str2)

assembleFunCode [] = return ""


assembleTopDef :: Code4Function -> IO()
assembleTopDef (argtypes,name,((label,inst):bs),vars,temps,strList,params)  = do
	putStr (functionStrings name strList)
	let (intro,off1,off2,off3,move) =functionIntro argtypes vars temps params
	putStr ("    .globl  "++(ciapkasReplace name)++"\n"++(ciapkasReplace name)++":\n"++"."++(ciapkasReplace label)++":\n    pushq\t %rbp\n    movq\t %rsp, %rbp\n"++intro)
	let (str1,_) = runState (assembleInstrs inst) (name,off1,off2,off3)
	let (str2,_) = runState (assembleFunCode (init bs)) (name,off1,off2,off3)
	putStr (str1++str2)
	putStr ("."++(ciapkasReplace name)++"_END:\n"++(functionOutro move temps))


assembleWhole :: [Code4Function] -> IO ()
assembleWhole (f:fs) = do
	assembleTopDef f
	assembleWhole fs

assembleWhole [] = return ()
