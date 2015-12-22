module Main(main) where

--pytania : może być while(true)?
--może być parametr funkcji typu void?
--czy może być zmienna typu void?


--TODO można użyć update mapy a nie insert przy zmienianiu wartości





import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map

import Latte.Abs
import Latte.Lex
import Latte.Par
import Latte.ErrM
import Latc_basic
import Latc_ExpTypeVal

--TODO poprawny format wypisywania error - bez nazwy programu
--TODO brak pliku wynikowego jak error

checkVoidArguments :: String -> [Arg] -> StEnv ()
checkVoidArguments name ((Arg t (PIdent ((x,y),_))):args) =
	case t of
		Void -> error ("error in line "++show(x)++", column "++show(y)++" function "++show(name)++" cannot have a void argument")
		_ -> checkVoidArguments name args

checkVoidArguments _ [] = return ()


argTypes :: [Arg] -> [Type]
argTypes ((Arg t pi):args) = t : (argTypes args)
argTypes [] = []


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
				-- Just (_) -> error "???"
				--Nothing -> error "???"

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
--	env <- ask
--	return env
	 
	 
insertVar :: Type -> Val -> ((Int,Int),String,Int) -> [Item]-> StEnv Env
insertVar t val (l,name,level) its = do
	st <- getSt	
	loc <- newLoc
	let s = Map.insert loc (t,val) st
	putSt s
	env2 <- (local (Map.insert name (l,loc,level)) (checkDecl t its level))
	return env2

duplicateAndAssVar :: String -> Val -> Int -> Bool -> StEnv Env	--lokacja
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


	 
checkDecl :: Type -> [Item] -> Int -> StEnv Env		--TODO czy potrzebuje dodatkowego level

checkDecl t ((Init (PIdent ((x,y),name)) exp):its) level = do
	env <- ask
	st <- getSt
	case t of 
		Void -> error ("error in line "++show(x)++", column "++show(y)++" variable cannot be of void type")
		_ -> do
			(type2,val) <- checkExpTypeVal exp
			if type2==t
				then case (Map.lookup name env) of
					Just ((x1,y1),_,level2) -> if level==level2
						then error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
						else insertVar t val ((x,y),name,level) its
					_ -> insertVar t val ((x,y),name,level) its
				else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++show(name)++" of different type")

			
checkDecl t ((NoInit (PIdent ((x,y),name))):its) level = do
	env <- ask
	st <- getSt
	case t of 
		Void -> error ("error in line "++show(x)++", column "++show(y)++" variable cannot be of void type")
		_ -> case (Map.lookup name env) of
			Just ((x1,y1),_,level2) -> if level==level2
											then error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" redefined\n previously defined in line "++show(x1)++", column "++show(y1))	
											else insertVar t (defaultVal t) ((x,y),name,level) its
			_ -> insertVar t (defaultVal t) ((x,y),name,level) its


checkDecl _ [] _ = ask


checkStmt :: Stmt ->Type -> Int -> Int -> Bool -> StEnv Env			--wyrażenie -> typ funkcji -> poziom -> poziom bezpieczny -> nadpis globalny (czy może być osiągalne)

checkStmt Empty _ _ _ _ = ask
{-|
checkStmt (BStmt (Block bl)) ft level slevel b= do
	checkBlock bl ft (level+1) slevel b
	ask
	-}
checkStmt (Decl t l) _ level _ _ = checkDecl t l level		--TODO czy slevel, i czy b

checkStmt (Ass (PIdent ((x,y),name)) exp) _ _ slevel b= do	--jeśli przypisujemy na zmienną z level<slevel, to źle TODO przetestować, tutaj na prawdę użyć b
	env <- ask
	st <- getSt
	(type2,val) <- checkExpTypeVal exp
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,level2) -> case (Map.lookup loc st) of
			Just (t,_) -> if t==type2
				then if level2<slevel 
					then do
						duplicateAndAssVar name val slevel b
					else do
						let s = Map.insert loc (t,val) st
						putSt s
						ask
				else error ("error in line "++show(x)++", column "++show(y)++" assigment to variable "++show(name)++" defined in line "++show(x1)++", column "++show(y1)++" of different type")
			--Nothing -> error "???"

checkStmt (Incr (PIdent ((x,y),name))) _ _ slevel b = do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,level2) -> case (Map.lookup loc st) of
			Just (Int,Nothing) -> return env
			Just (Int,Just (Right n))-> if level2<slevel 
				then do
					duplicateAndAssVar name (Just (Right (n+1))) slevel b
				else do
					let s = Map.insert loc (Int,Just (Right (n+1))) st
					putSt s
					ask			
			_ -> error ("error in line "++show(x)++", column "++show(y)++" incrementation of variable "++show(name)++" defined in line "++show(x1)++", column "++show(y1)++" of non-integer type")	

checkStmt (Decr (PIdent ((x,y),name))) _ _ slevel b = do
	env <- ask
	st <-getSt
	case (Map.lookup name env) of
		Nothing -> error ("error in line "++show(x)++", column "++show(y)++" variable "++show(name)++" not declared")
		Just ((x1,y1),loc,level2) -> case (Map.lookup loc st) of
			Just (Int,Nothing) -> return env
			Just (Int,Just (Right n))-> if level2<slevel 
				then do
					duplicateAndAssVar name (Just (Right (n-1))) slevel b
				else do
					let s = Map.insert loc (Int,Just (Right (n-1))) st
					putSt s
					ask			
			_ -> error ("error in line "++show(x)++", column "++show(y)++" decrementation of variable "++show(name)++" defined in line "++show(x1)++", column "++show(y1)++" of non-integer type")

{- |
checkStmt (Ret (PReturn ((x,y),_)) exp) ft _ _ _ = case ft of
	Void -> error ("error in line "++show(x)++", column "++show(y)++"return with argument in procedure")
	_ -> do
		(type2,_) <- checkExpTypeVal exp
		if(type2==ft)
			then ask
			else error ("error in line "++show(x)++", column "++show(y)++" return with wrong type")

checkStmt (VRet (PReturn ((x,y),_))) ft _ _ _ = case ft of
	Void -> ask
	_ -> error ("error in line "++show(x)++", column "++show(y)++"return without argument in non-void function")
-}
{-|
checkStmt (Cond (PIf ((x,y),_)) exp stm) ft level slevel b= do			--TODO tutaj wejście do bloku warunkowego lub False dać level+2 i slevel=level+1
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
			_ -> case val of
				Just (Left (Right True)) -> checkStmt stm ft level slevel b	--TODOTODO to się wykona zawsze - można usunąć if warunek itp. jak blok to ok, jak deklaracja to się wywal
				Just (Left (Right False)) -> checkStmt stm ft (level+1) (level+1) False	--TODOTODO to się i tak nie wykona, ale warto sprawdzić otypowanie - można by zrobić pierwszą optymalizację 
				Nothing -> checkStmt stm ft (level+1) (level+1)	b					--TODOTODO to się może wykonać - jak deklaracja to się wywal, jak przypisanie, to daj wartość na nieokreśloną, jak blok to dalej ... z nieokreślonością, jak if czy coś to ???, czyli ustawić slevel jako level+1,,,
		_ -> error ("error in line "++show(x)++", column "++show(y)++"if condition of non-boolean type")
		
checkStmt (CondElse (PIf ((x,y),_)) exp stm stm2) ft level slevel b = do	
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
			_ -> case stm2 of
				Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
				_ -> case val of
					Just (Left (Right True)) -> do
						env2 <- checkStmt stm ft level slevel b
						checkStmt stm2 ft (level+1) (level+1) False		--TODO przecież tutaj zmieniam stan zewnętrzny, a nie powinienem
						return env2	--to właściwie nie ma znaczenia - jedyny przypadek w którym zmieniłoby się środowisko to deklaracja, która jest zakazana nawet w if(true)
					Just (Left (Right False)) -> do
						checkStmt stm ft (level+1) (level+1) False		--TODO przecież tutaj zmieniam stan zewnętrzny, a nie powinienem
						env2 <- checkStmt stm2 ft level slevel b
						return env2
					Nothing -> do
						checkStmt stm ft (level+1) (level+1) b
						checkStmt stm2 ft (level+1) (level+1) b
						ask
		_ -> error ("error in line "++show(x)++", column "++show(y)++"if condition of non-boolean type")



checkStmt (While (PWhile ((x,y),_)) exp stm) ft level slevel b = do	-- TODO można sprawdzić, czy pętla jest bezwzględnie prawdziwa - wtedy nie musi być returna po niej itp.
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in while")
			_ -> case val of 
				Just (Left (Right False)) -> checkStmt stm ft (level+1) (level+1) False	--false więc ani razu nie zrobi => tylko sprawdzić typowanie - jak if
				_ -> do
					checkStmt stm ft (level+1) (level+1) b --chyba ze 2 razy
					--TODOTODO teraz jak mogły zostać zmienione - nadpisane różne rzeczy jak stm w nowym stanie jest true - to zawsze będzie true - pętla nieskończona
					--  - nie musi być returna po niej
					ask
					--TODOTODOTODO tutaj 2
		_ -> error ("error in line "++show(x)++", column "++show(y)++"while condition of non-boolean type")
	--TODO może sprawdź na ifie, jako if niepewny, nawet jak true, następnie w envie zwróconym jeszcze raz while
 -}
{-| 
checkStmt (SExp exp) _ _ _ _= do
	checkExpTypeVal exp
	ask
-}

--checkStmt _ _ _ _ _= ask --TODO usunąć








checkBlock :: [Stmt] -> Type -> Int -> Int -> Bool -> StEnv Bool

checkBlock ((BStmt (Block bl)):stmts) ft level slevel b = do
	b2 <- checkBlock bl ft (level+1) slevel b
	checkBlock stmts ft level level b2

checkBlock ((Ret (PReturn ((x,y),_)) exp):stmts) ft level _ _ = case ft of
	Void -> error ("error in line "++show(x)++", column "++show(y)++"return with argument in procedure")
	_ -> do
		(type2,_) <- checkExpTypeVal exp
		if(type2==ft)
			then do
				checkBlock stmts ft level level False
				return False		
			else error ("error in line "++show(x)++", column "++show(y)++" return with wrong type")

checkBlock ((VRet (PReturn ((x,y),_))):stmts) ft level _ _ = case ft of
	Void -> do
		checkBlock stmts ft level level False
		return False
	_ -> error ("error in line "++show(x)++", column "++show(y)++"return without argument in non-void function")


checkBlock ((Cond (PIf ((x,y),_)) exp stm):stmts) ft level slevel b= do			--TODO tutaj wejście do bloku warunkowego lub False dać level+2 i slevel=level+1
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
			_ -> case val of
				Just (Left (Right True)) -> do 
					b2 <- checkBlock [stm] ft level slevel b	--TODOTODO to się wykona zawsze - można usunąć if warunek itp. jak blok to ok, jak deklaracja to się wywal
					checkBlock stmts ft level level b2
				Just (Left (Right False)) -> do
					checkBlock [stm] ft (level+1) (level+1) False	--TODOTODO to się i tak nie wykona, ale warto sprawdzić otypowanie - można by zrobić pierwszą optymalizację 
					checkBlock stmts ft level level b
				Nothing -> do 
					checkBlock [stm] ft (level+1) (level+1) b					--TODOTODO to się może wykonać - jak deklaracja to się wywal, jak przypisanie, to daj wartość na nieokreśloną, jak blok to dalej ... z nieokreślonością, jak if czy coś to ???, czyli ustawić slevel jako level+1,,,
					checkBlock stmts ft level level b
		_ -> error ("error in line "++show(x)++", column "++show(y)++" if condition of non-boolean type")

checkBlock ((CondElse (PIf ((x,y),_)) exp stm stm2):stmts) ft level slevel b = do	
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
			_ -> case stm2 of
				Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in if")
				_ -> case val of
					Just (Left (Right True)) -> do
						b2 <- checkBlock [stm] ft level slevel b
						checkBlock [stm2] ft (level+1) (level+1) False		--TODO przecież tutaj zmieniam stan zewnętrzny, a nie powinienem
						checkBlock stmts ft level level b2	--to właściwie nie ma znaczenia - jedyny przypadek w którym zmieniłoby się środowisko to deklaracja, która jest zakazana nawet w if(true)
					Just (Left (Right False)) -> do
						checkBlock [stm] ft (level+1) (level+1) False		--TODO przecież tutaj zmieniam stan zewnętrzny, a nie powinienem
						b2 <- checkBlock [stm2] ft level slevel b
						checkBlock stmts ft level level b2
					Nothing -> do
						b2 <- checkBlock [stm] ft (level+1) (level+1) b
						b3 <- checkBlock [stm2] ft (level+1) (level+1) b
						checkBlock stmts ft level level (b2||b3)
		_ -> error ("error in line "++show(x)++", column "++show(y)++"if condition of non-boolean type")



--TODOTODOTODO tutaj 1
--TODOTODOTODO while -> kolejna zmienna mówiąca, że nie możemy powiedzieć nic o wartościach zmiennych w pętli, bo mogły się zmienić po poprzednich przejściach 
-- można by zrobić fix point, ale jest to bardziej skomplikowane - coś w stylu z while wyciągnąć pierwszy przebieg pętli -> zamienić na if + pętla na końcu
-- wtedy już wiadomo, na które zmienne nic się nie przypisuje i można by je traktować jako znane, a nie jako potencjalny Nothing

checkBlock ((While (PWhile ((x,y),_)) exp stm):stmts) ft level slevel b = do	-- TODO można sprawdzić, czy pętla jest bezwzględnie prawdziwa - wtedy nie musi być returna po niej itp.
	(type1,val) <- checkExpTypeVal exp
	case type1 of
		Bool -> case stm of
			Decl _ _ -> error ("error in line "++show(x)++", column "++show(y)++" bare declaration in while")
			_ -> case val of 
				Just (Left (Right False)) -> do
					checkBlock [stm] ft (level+1) (level+1) False	--false więc ani razu nie zrobi => tylko sprawdzić typowanie - jak if
					checkBlock stmts ft level slevel b
				_ -> do
					whileStFixPoint stm ft (level+1) b				
					(_,val) <- checkExpTypeVal exp
					case val of
						Just (Left (Right True)) -> do						--teraz już po fix poincie na pewno jest dobrze
							checkBlock stmts ft level level False
							return False
						_ -> checkBlock stmts ft level level b												--teraz czy false czy nie nie jest ważne
					--TODOTODO teraz jak mogły zostać zmienione - nadpisane różne rzeczy jak stm w nowym stanie jest true - to zawsze będzie true - pętla nieskończona
					--  - nie musi być returna po niej
					--TODOTODOTODO tutaj 2
		_ -> error ("error in line "++show(x)++", column "++show(y)++" while condition of non-boolean type")

checkBlock ((SExp exp):stmts) ft level slevel b = do
	case exp of
		EApp (PIdent ((x,y),"error")) []-> do			--TODO jeśli będzie obiektowość i możliwość nadpisania error, to trzeba sprawdzić, że to ten z poziomu 0
			checkBlock stmts ft level level False
			return False
		_ -> do 
			checkExpTypeVal exp
			checkBlock stmts ft level slevel b
			
checkBlock (stm:stmts) ft level slevel b = do
	env <- checkStmt stm ft level slevel b
	(local (\x -> env) (checkBlock stmts ft level slevel b))
				
checkBlock  [] _ _ _ b = return b




whileStFixPoint :: Stmt -> Type -> Int -> Bool -> StEnv ()
whileStFixPoint stm ft level b = do
	st <- getSt
	checkBlock [stm] ft level level b
	st2 <- getSt
	if Map.isSubmapOf st st2
		then return ()
		else do
		whileStFixPoint stm ft level b






checkFunction :: TopDef -> StEnv ()
checkFunction (FnDef t (PIdent ((x,y),name)) args (Block bl)) = do
	env <- checkArgs args
	b <- (local (\x -> env) (checkBlock bl t 2 0 True))
	case t of
	 Void -> return ()
	 _ -> if b
		then error ("error non-void function "++show(name)++" declared in line "++show(x)++", column "++show(y)++" possibly could end without return")
		else return ()
	return ()

checkRest :: [TopDef] -> StEnv ()
checkRest (f:fs) = do
	checkFunction f
	checkRest fs
	return ()

checkRest [] = return ()










compileFunction :: TopDef -> StEnv ()
compileFunction (FnDef t (PIdent ((x,y),name)) args block) = return ()


compileFunctions ::[TopDef] ->StEnv ()

compileFunctions (f:fs) = do
	compileFunction f
	compileFunctions fs
	return ()
	
compileFunctions [] = return ()

compileProgram :: Program -> StEnv ()
compileProgram (Program p) = do
	env <- checkFunctionSignatures p
	(local (\x ->env) (checkRest p))
	compileFunctions p
	return ()

compileWhole :: Program -> IO ()
compileWhole prog = do
	(_,(st,_)) <- runStateT (runReaderT (compileProgram prog) predefinedEnv) predefinedSt
	hPutStrLn stderr ("OK\n"++(show st))
	return ()


lexerErrCheck :: Err Program -> IO()
lexerErrCheck (Ok e) = do
	Control.Exception.catch (compileWhole e) (\msg -> hPutStrLn stderr $ "ERROR\n"++show(msg::Control.Exception.SomeException))
	exitWith (ExitFailure 1)

lexerErrCheck (Bad s) = do
	hPutStrLn stderr ("ERROR\n" ++ s)
	exitWith (ExitFailure 1)


executeOnFile :: String -> IO()
executeOnFile s = lexerErrCheck (pProgram (myLexer s))

main = do
	args <- getArgs
	case args of
		[f] -> readFile f >>= executeOnFile
