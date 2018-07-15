module QuickLTLCheck where
import LTL
import Interpreter
import DSL
import AST
import CodeLibrary
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

runProg :: Prog -> [Env Value]
runProg p = evalState (mapM execStmt p >>= \r -> return $ concat r) inistate

--runProg' :: Prog -> ([Env Value], ProgState)
runProg' p = runState (mapM execStmt p >>= \r -> return $ concat r) inistate
             

code :: State ASTSt () -> Prog
code st = head $ map (\(a,b,c) -> b)((execState st (Map.empty,[],[Map.empty])):[])

inistate :: ProgState
inistate = (Map.empty, Map.fromList [(nun2digits e ++ "p",VInt 0)| e <- [1..32]],0)

inistate' :: ProgState
inistate' = (Map.empty, Map.empty,0)


nun2digits :: Int -> String
nun2digits e
   | (0 < e) && (e < 10) = "0" ++ (show e) 
   | otherwise = show e 


cond :: Interp Value
cond "a" i = i < (VInt 4) 

sem :: Interp Value
sem "07p" i = i == (VInt 5)
sem "08p" i = i == (VInt 5)

sem' :: Interp Value
sem' "06p" i = i == (VInt 5)
sem' "09p" i = i == (VInt 0)

prop :: LTL
prop = G ((LTL.Not (Atom "07p")) :|: (Atom "08p"))

prop'' :: LTL
prop'' = G ((LTL.Not (Atom "06p")) :|: (Atom "09p"))

cond' :: Interp Value
cond' "temperatura" i = i < (VInt 100)

prop' :: LTL
prop' = G (Atom "temperatura") 

checkProg :: Prog -> Interp Value -> LTL -> (Bool, Maybe (Env Value))
checkProg p i ltl = checkLTLZ i ltl (runProg p)

partialCheckProg :: Int -> Prog -> Interp Value -> LTL -> (Bool, Maybe (Env Value))
partialCheckProg s p i ltl = checkLTLZ i ltl (take s $ runProg p)
