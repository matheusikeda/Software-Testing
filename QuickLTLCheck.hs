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
inistate = (Map.empty, Map.empty,0)

cond :: Interp Value
cond s i = s == "z" && i < (VInt 6)

prop :: LTL
prop = G (Atom "z")

c1 :: Interp Value
c1 "pin9" i = i == (VInt 0)
c1 "pin6" i = i == (VInt 5)
c1 "pin8" i = i == (VInt 0)
c1 "pin5" i = i == (VInt 5)
c1 "pin10" i = i == (VInt 0)
c1 "pin7" i = i == (VInt 5)
c1 _ _ = False

prop' :: LTL
prop' = G (LTL.Not (Atom "pin6") :|: (Atom "pin9"))

checkProg :: Prog -> Interp Value -> LTL -> Bool
checkProg p i ltl = checkLTL i ltl (runProg p)

partialCheckProg :: Int -> Prog -> Interp Value -> LTL -> Bool
partialCheckProg s p i ltl = checkLTL i ltl (take s (runProg p))