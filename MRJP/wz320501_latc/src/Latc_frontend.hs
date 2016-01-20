module Latc_frontend where

--moduł sprawdzający poprawność programu

import Control.Monad.Reader
import Control.Monad.State
import System.Environment

import qualified Data.Map as Map

import Latte.Abs
import Latc_basic
import Latc_ExpTypeVal

--funkcja sprawdza, czy program nie używa void jako argumentu funkcji
checkVoidArguments :: String -> [Arg] -> StEnv ()
checkVoidArguments name ((Arg t (PIdent ((x,y),_))):args) =
	case t of
		Void -> error ("error in line "++show(x)++", column "++show(y)++" function "++show(name)++" cannot have a void argument")
		_ -> checkVoidArguments name args

checkVoidArguments _ [] = return ()



--funkcja sprawdza poprawność deklaracji funkcji
checkFunctionSignatures :: [TopDef] -> StEnv Env
checkFunctionSignatures ((FnDef t (PIdent ((x,y),name)) args block):fs) = do
	checkVoidArguments name args
	env <- ask														
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> do
			loc <- newLoc
			let s = Map.insert loc (Fun t (argTypes args),Nothing) st 
			putSt s	
			env2 <- (local (Map.insert name ((x,y),loc,0)) (checkFunctionSignatures fs))			
			return env2
		Just ((x1,y1),_,_) -> error ("error in line "++show(x)++", column "++show(y)++" function "++show(name)++" previously defined in line "++show(x1)++", column "++show(y1))

checkFunctionSignatures [] = do
	env <- ask
	case (Map.lookup "main" env) of
		Nothing -> error ("error no main function")
		Just ((x,y),(loc),_) -> do
			st <- getSt
			case (Map.lookup loc st) of
				Just (Fun Int [],_) -> do return env
				Just (Fun _ (h:hs),_) -> error ("error main in line "++show(x)++" function cannot have arguments")
				Just (Fun _ _,_)	-> error ("error main function in line "++show(x)++" must return int")


--funkcja sprawdza poprawność argumentów funkcji (unikalność nazw)
checkArgs :: [Arg] -> StEnv Env

checkArgs ((Arg t (PIdent ((x,y),name))):args) = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Just ((x1,y1),_,1) -> error ("error in line "++show(x)++", column "++show(y)++" argument "++show(name)++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
		_ -> do
			loc <- newLoc
			let s = Map.insert loc (t,Nothing) st 
			putSt s
			env2 <- (local (Map.insert name ((x,y),loc,1)) (checkArgs args))			
			return env2

checkArgs [] = ask
	 
--wstawienie zmiennej do środowiska i stanu
insertVar :: Type -> Val -> ((Int,Int),String,Int) -> [Item]-> StEnv (Env,[Item])
insertVar t val (l,name,level) its = do
	st <- getSt	
	loc <- newLoc
	let s = Map.insert loc (t,val) st
	putSt s
	(local (Map.insert name (l,loc,level)) (checkDecl t its level))
	
--duplikowanie zmiennych używane przy wchodzeniu do bloków - aby były widoczne, ale mogły być nadpisane
duplicateAndAssVar :: String -> Val -> Int -> Bool -> StEnv Env
duplicateAndAssVar name val newlevel b= do
	env <- ask
	st <- getSt
	case Map.lookup name env of
		Just ((x1,y1),loc,_)-> case (Map.lookup loc st) of
			Just (t,_) -> do			
				let s1 = if b
					then Map.insert loc (t,Nothing) st
					else st
				loc2 <-newLoc	
				let s2 = Map.insert loc2 (t,val) s1
				putSt s2
				return (Map.insert name ((x1,y1),loc2,newlevel) env)
			_ -> error "???"
		_ -> error "???"

--sprawdzenie deklaracji zmiennych + usunięcie NoInit - poprzez zmienienie w Init wartość domyślna
checkDecl :: Type -> [Item] -> Int -> StEnv (Env,[Item])

checkDecl t ((Init (PIdent ((x,y),name)) exp):its) level = do
	env <- ask
	st <- getSt
	case t of 
		Void -> error ("error in line "++show(x)++", column "++show(y)++" variable cannot be of void type")
		_ -> do
			(type2,val,nexp) <- checkExpTypeVal exp
			if type2==t
				then case (Map.lookup name env) of
					Just ((x1,y1),_,level2) -> if level==level2
						then error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
						else do
							(env2,nits) <- insertVar t val ((x,y),name,level) its
							return (env2,((Init (PIdent ((x,y),name)) nexp):nits))
					_ -> do
						(env2,nits) <- insertVar t val ((x,y),name,level) its
						return (env2,((Init (PIdent ((x,y),name)) nexp):nits))		
				else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++show(name)++" of different type "++(wrongType t type2))

checkDecl t ((NoInit (PIdent ((x,y),name))):its) level = do
	env <- ask
	st <- getSt
	case t of 
		Void -> error ("error in line "++show(x)++", column "++show(y)++" variable cannot be of void type")
		_ -> case (Map.lookup name env) of
			Just ((x1,y1),_,level2) -> if level==level2
											then error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
											else do
												(env2,nits) <- insertVar t (defaultVal t) ((x,y),name,level) its
												return (env2,((Init (PIdent ((x,y),name)) (defaultValExpr t)):nits))
			_ -> do
				(env2,nits) <- insertVar t (defaultVal t) ((x,y),name,level) its
				return (env2,((Init (PIdent ((x,y),name)) (defaultValExpr t)):nits))
				
checkDecl _ [] _ = do
	env <- ask
	return (env,[])

--sprawdzenie poprawności instrukcji + uproszczenie
checkStmt :: Stmt ->Type -> Int -> Int -> Bool -> StEnv (Env,Stmt)

checkStmt Empty _ _ _ _ = do
	env <- ask
	return (env,Empty)

checkStmt (Decl t l) _ level _ _ = do
	(env,nl) <- checkDecl t l level
	return (env,(Decl t nl))

checkStmt (Ass (PIdent ((x,y),name)) exp) _ _ slevel b= do
	env <- ask
	st <- getSt
	(type2,val,nexp) <- checkExpTypeVal exp
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,level2) -> case (Map.lookup loc st) of
			Just (t,_) -> if t==type2
				then if level2<slevel 
					then do
						env2 <- duplicateAndAssVar name val slevel b
						return (env2,(Ass (PIdent ((x,y),name)) nexp))
					else do
						let s = Map.insert loc (t,val) st
						putSt s
						env2 <- ask
						return (env2,(Ass (PIdent ((x,y),name)) nexp))
				else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++show(name)++" defined in line "++show(x1)
							++", column "++show(y1)++" of different type "++(wrongType t type2))

checkStmt (ArrAss (PIdent ((x,y),name)) exp1 exp2) _ _ _ b= do
	env <- ask
	st <- getSt
	(type1,_,nexp1) <- checkExpTypeVal exp1
	(type2,_,nexp2) <- checkExpTypeVal exp2
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" array "++show(name)++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Array t,_) -> if (type1==Int)
				then if (t==type2)
					then return (env,(ArrAss (PIdent ((x,y),name)) nexp1 nexp2))
					else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++show(name)++" defined in line "++show(x1)
							++", column "++show(y1)++" of different type "++(wrongType t type2))
				else error ("array index of non integer type at line "++show(x)++", column "++show(y))
			_ -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" is not an array")


checkStmt (Incr (PIdent ((x,y),name))) _ _ slevel b = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,level2) -> case (Map.lookup loc st) of
			Just (Int,Nothing) -> return (env,(Incr (PIdent ((x,y),name))))
			Just (Int,Just (Right n))-> if level2<slevel 
				then do
					env2 <- duplicateAndAssVar name (Just (Right (n+1))) slevel b
					return (env2,(Incr (PIdent ((x,y),name))))	
				else do
					let s = Map.insert loc (Int,Just (Right (n+1))) st
					putSt s
					env2 <- ask
					return (env2,(Incr (PIdent ((x,y),name))))		
			_ -> error ("error in line "++show(x)++", column "++show(y)++" incrementation of variable "++show(name)++" defined in line "++show(x1)
						++", column "++show(y1)++" of non-integer type")	

checkStmt (Decr (PIdent ((x,y),name))) _ _ slevel b = do
	env <- ask
	st <- getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,level2) -> case (Map.lookup loc st) of
			Just (Int,Nothing) -> return (env,(Decr (PIdent ((x,y),name))))	
			Just (Int,Just (Right n))-> if level2<slevel 
				then do
					env2 <- duplicateAndAssVar name (Just (Right (n-1))) slevel b
					return (env2,(Decr (PIdent ((x,y),name))))	
				else do
					let s = Map.insert loc (Int,Just (Right (n-1))) st
					putSt s
					env2 <- ask
					return (env2,(Decr (PIdent ((x,y),name))))				
			_ -> error ("error in line "++show(x)++", column "++show(y)++" decrementation of variable "++show(name)++" defined in line "++show(x1)
						++", column "++show(y1)++" of non-integer type")



--sprawdzenie poprawności bloku + uproszczenie , sprawdzane są tutaj również instrukcje mogące wpłynąć na osiągalność dalszej części bloku (return,while,if,error())
checkBlock :: [Stmt] -> Type -> Int -> Int -> Bool -> StEnv (Bool,[Stmt])

checkBlock ((BStmt (Block bl)):stmts) ft level slevel b = do
	(b2,nbl) <- checkBlock bl ft (level+1) slevel b
	(b3,nstmts) <- checkBlock stmts ft level level b2
	if b2
		then return (b3,((BStmt (Block nbl)):nstmts))
		else return (False,[BStmt (Block nbl)])

checkBlock ((Ret (PReturn ((x,y),_)) exp):stmts) ft level _ _ = case ft of
	Void -> error ("error in line "++show(x)++", column "++show(y)++"return with argument in procedure")
	_ -> do
		(type2,_,nexp) <- checkExpTypeVal exp
		if(type2==ft)
			then do
				checkBlock stmts ft level level False
				return (False,[(Ret (PReturn ((x,y),"return")) nexp)])		
			else error ("error in line "++show(x)++", column "++show(y)++" return with wrong type")

checkBlock ((VRet (PReturn ((x,y),_))):stmts) ft level _ _ = case ft of
	Void -> do
		checkBlock stmts ft level level False
		return (False,[(VRet (PReturn ((x,y),"return")))])
	_ -> error ("error in line "++show(x)++", column "++show(y)++"return without argument in non-void function")


checkBlock ((Cond (PIf ((x,y),_)) exp stm):stmts) ft level slevel b= do
	(type1,val,nexp) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
			_ -> case val of
				Just (Left (Right True)) -> do 
					(b2,[nstm]) <- checkBlock [stm] ft level slevel b
					(b3,nstmts) <- checkBlock stmts ft level level b2
					if b2
						then return (b3,(nstm:nstmts))				
						else return (False,[nstm])
				Just (Left (Right False)) -> do
					checkBlock [stm] ft (level+1) (level+1) False
					checkBlock stmts ft level level b
				Nothing -> do 
					(_,[nstm]) <- checkBlock [stm] ft (level+1) (level+1) b
					(b2,nstmts) <- checkBlock stmts ft level level b
					return (b2,((Cond (PIf ((x,y),"if")) nexp nstm):nstmts))
		_ -> error ("error in line "++show(x)++", column "++show(y)++" if condition of non-boolean type")

checkBlock ((CondElse (PIf ((x,y),_)) exp stm1 stm2):stmts) ft level slevel b = do	
	(type1,val,nexp) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm1 of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
			_ -> case stm2 of
				Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
				_ -> case val of
					Just (Left (Right True)) -> do
						(b2,[nstm]) <- checkBlock [stm1] ft level slevel b
						checkBlock [stm2] ft (level+1) (level+1) False
						(b3,nstmts) <- checkBlock stmts ft level level b2
						if b2
							then return (b3,(nstm:nstmts))				
							else return (False,[nstm])					
					Just (Left (Right False)) -> do
						checkBlock [stm1] ft (level+1) (level+1) False
						(b2,[nstm]) <- checkBlock [stm2] ft level slevel b
						(b3,nstmts) <- checkBlock stmts ft level level b2
						if b2
							then return (b3,(nstm:nstmts))				
							else return (False,[nstm])						
					Nothing -> do
						(b2,[nstm1]) <- checkBlock [stm1] ft (level+1) (level+1) b
						(b3,[nstm2]) <- checkBlock [stm2] ft (level+1) (level+1) b
						(b4,nstmts) <- checkBlock stmts ft level level (b2||b3)
						if b2||b3
							then return (b4,((CondElse (PIf ((x,y),"if")) nexp nstm1 nstm2):nstmts))			
							else return (False,[(CondElse (PIf ((x,y),"if")) nexp nstm1 nstm2)])
		_ -> error ("error in line "++show(x)++", column "++show(y)++"if condition of non-boolean type")

checkBlock ((While (PWhile ((x,y),_)) exp stm):stmts) ft level slevel b = do
	(type1,val,_) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in while")
			_ -> case val of 
				Just (Left (Right False)) -> do
					checkBlock [stm] ft (level+1) (level+1) False
					checkBlock stmts ft level slevel b
				_ -> do
					(b2,nstm) <- whileStFixPoint stm ft (level+1) b
					if b2	--jeśli b2 jest false, to znaczy, że już w pierwszym przebiegu nastąpi return lub wewnętrzna pewna pętla nieskończona --TODOTODO a co jak nie wykona się ani razu - chodzi o zwrot tego czy koniec czy nie
						then do (_,val,nexp) <- checkExpTypeVal exp
							case val of
								Just (Left (Right True)) -> do
									checkBlock stmts ft level level False
									return (False,[(While (PWhile ((x,y),"while")) ELitTrue nstm)])
								_ -> do
									(b2,nstmts) <- checkBlock stmts ft level level b
									return (b2,((While (PWhile ((x,y),"while")) nexp nstm):nstmts))
						else return (False,[(Cond (PIf ((x,y),"if")) ELitTrue nstm)])	--TODOTODO powinno chyba zwrócić też resztę - bo mógł się w ogóle nie wykonać
		_ -> error ("error in line "++show(x)++", column "++show(y)++" while condition of non-boolean type")

checkBlock ((ForEach (PFor ((x,y),_)) t (PIdent ((x1,y1),var)) (PIdent ((x2,y2),arr)) stm):stmts) ft level slevel b = do
	env <- ask
	st <- getSt
	case (Map.lookup arr env) of
		Nothing -> error ("error in line "++show(x2)++", column "++show(y2)++" array "++show(arr)++" not declared")
		Just ((x1,y1),loc,_) -> case (Map.lookup loc st) of
			Just (Array t2,_) -> if (t==t2)
				then case stm of
					Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in for")
					_ -> do
						(env2,_) <- insertVar t Nothing ((x1,y1),var,level+1) []
						(_,nstm) <- local (\x->env2) (whileStFixPoint stm ft (level+1) b)
						(b2,nstmts) <- checkBlock stmts ft level level b
						return (b2,((ForEach (PFor ((x,y),"for")) t (PIdent (((x1,y1),var))) (PIdent (((x2,y2),arr))) nstm):nstmts))
				else error ("error in line "++show(x1)++", column "++show(y1)++" variable "++(show var)++" of wrong type "++(wrongType t2 t))
			_ -> error ("attempt to use non array variable as array at line "++show(x2)++", column "++show(y2))		

checkBlock ((SExp exp):stmts) ft level slevel b = do
	case exp of
		EApp (PIdent ((x,y),"error")) []-> do
			checkBlock stmts ft level level False
			return (False,[(SExp (EApp (PIdent ((0,y),"error")) []))])
		_ -> do 
			(_,_,nexp) <- checkExpTypeVal exp
			(b2,nstmts) <- checkBlock stmts ft level slevel b
			return (b2,((SExp nexp):nstmts))
			
checkBlock (stm:stmts) ft level slevel b = do
	(env,nstm) <- checkStmt stm ft level slevel b
	(b2,nstmts) <- (local (\x -> env) (checkBlock stmts ft level slevel b))
	return (b2,(nstm:nstmts))
				
checkBlock  [] _ _ _ b = return (b,[])

--funkcja wyliczająca stan wykonania while - wykrycie pewnego while(true)
whileStFixPoint :: Stmt -> Type -> Int -> Bool -> StEnv (Bool,Stmt)
whileStFixPoint stm ft level b = do
	st <- getSt
	(b2,[nstmt]) <- checkBlock [stm] ft level level b
	if b2
		then do st2 <- getSt
			if Map.isSubmapOf st st2
				then return (b2,nstmt)
				else whileStFixPoint stm ft level b2
		else return (False,nstmt)



--sprawdzenie poprawności wnętrza funkcji
checkFunction :: TopDef -> StEnv TopDef
checkFunction (FnDef t (PIdent ((x,y),name)) args (Block bl)) = do
	env <- checkArgs args
	(b,nbl) <- (local (\x -> env) (checkBlock bl t 2 0 True))
	case t of
	 Void -> return (FnDef t (PIdent ((x,y),name)) args (Block nbl))
	 _ -> if b
		then error ("error non-void function "++show(name)++" declared in line "++show(x)++", column "++show(y)++" possibly could end without return")
		else return (FnDef t (PIdent ((x,y),name)) args (Block nbl))

--drugi przebieg = sprawdzenie wszystkiego poza deklaracjami funkcji
checkRest :: [TopDef] -> StEnv [TopDef]
checkRest (f:fs) = do
	nf <- checkFunction f
	nfs <- checkRest fs
	return (nf:nfs)

checkRest [] = return []


checkProg :: [TopDef]-> StEnv [TopDef]
checkProg p = do
	env <- checkFunctionSignatures p
	np <- (local (\x ->env) (checkRest p))
	return np
