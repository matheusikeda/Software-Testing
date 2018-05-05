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

code :: State ASTSt () -> Prog
code st = head $ map (\(a,b,c) -> b)((execState st (Map.empty,[],[Map.empty])):[])

inistate :: ProgState
inistate = (Map.empty, Map.empty)

cond :: Interp Value
cond s i = s == "z" && i < (VInt 6)

prop :: LTL
prop = G (Atom "z")

checkProg :: Prog -> Interp Value -> LTL -> Bool
checkProg p i ltl = checkLTL i ltl (runProg p)

-- example :: Prog
-- example = [Declare "x" (ILit 10),
--            Declare "y" (ILit 5),
--            Declare "z" (ILit 0),
--            If (Equal  (Plus (EVar "x") (EVar "y")) (ILit 15)) [While (Lt (EVar "z") (ILit 5)) [(Assign "z" (Plus (EVar "z") (ILit 1)))]] Nothing]
