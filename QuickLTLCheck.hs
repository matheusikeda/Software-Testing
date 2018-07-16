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


testVal :: (Value -> Value -> Bool) -> String ->  Value -> Env Value -> Bool  
testVal p var val e = case (Map.lookup var e) of
                         Just  i -> p i val  
                         Nothing -> False

operVars :: (Value -> Value -> Bool) -> String -> String -> Env Value -> Bool
operVars p v1 v2 mp = case (Map.lookup v1 mp,Map.lookup v2 mp) of
                         (Just  i, Just j)  -> p i j  
                         _                 -> False

cond :: Interp Value
cond "x" mp = testVal (>) "x" (VFloat 0) mp  

sem :: Interp Value
sem "07p" mp = testVal (==) "07p" (VInt 5) mp
sem "08p" mp = testVal (==) "08p" (VInt 5) mp

p :: LTL
p = (F (Atom "x")) 

prop :: LTL
prop = G ((LTL.Not (Atom "07p")) :|: (Atom "08p"))

prop'' :: LTL
prop'' = G ((LTL.Not (Atom "06p")) :|: (Atom "09p"))

cond' :: Interp Value
cond' "temperatura" mp = testVal (<) "temperatura" (VInt 100) mp

prop' :: LTL
prop' = G (Atom "temperatura") 

checkProg :: Prog -> Interp Value -> LTL -> (Bool, Maybe (Env Value))
checkProg p i ltl = checkLTLZ i ltl (runProg p)

partialCheckProg :: Int -> Prog -> Interp Value -> LTL -> (Bool, Maybe (Env Value))
partialCheckProg s p i ltl = checkLTLZ i ltl (take s $ runProg p)
