module DSL where

import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map
import AST

type Env a = Map String a
type Interp a = String -> a -> Bool

(<+>) :: Value -> Value -> Value 
(<+>) (VInt v) (VInt v') = VInt (v + v')
(<+>) (VFloat v) (VFloat v') = VFloat (v + v')
(<+>) (VDouble v) (VDouble v') = VDouble (v + v')

(<->) :: Value -> Value -> Value 
(<->) (VInt v) (VInt v') = VInt (v - v')
(<->) (VFloat v) (VFloat v') = VFloat (v - v')
(<->) (VDouble v) (VDouble v') = VDouble (v - v')

(<**>) :: Value -> Value -> Value 
(<**>) (VInt v) (VInt v') = VInt (v * v')
(<**>) (VFloat v) (VFloat v') = VFloat (v * v')
(<**>) (VDouble v) (VDouble v') = VDouble (v * v')

(</>) :: Value -> Value -> Value 
(</>) (VInt v) (VInt v') = VInt (div v  v')
(</>) (VFloat v) (VFloat v') = VFloat (v / v')
(</>) (VDouble v) (VDouble v') = VDouble (v / v')

(<&&>) :: Value -> Value -> Value 
(<&&>) (VBool v) (VBool v') = VBool (v && v')

(<||>) :: Value -> Value -> Value 
(<||>) (VBool v) (VBool v') = VBool (v || v')

(<==>) :: Value -> Value -> Value 
(<==>) (VInt v) (VInt v') = VBool (v == v')
(<==>) (VFloat v) (VFloat v') = VBool (v == v')
(<==>) (VDouble v) (VDouble v') = VBool (v == v')
(<==>) (VBool v) (VBool v') = VBool (v == v')
(<==>) (VChar v) (VChar v') = VBool (v == v')

(<>>) :: Value -> Value -> Value 
(<>>) (VInt v) (VInt v') = VBool (v > v')
(<>>) (VFloat v) (VFloat v') = VBool (v > v')
(<>>) (VDouble v) (VDouble v') = VBool (v > v')

(<<>) :: Value -> Value -> Value 
(<<>) (VInt v) (VInt v') = VBool (v < v')
(<<>) (VFloat v) (VFloat v') = VBool (v < v')
(<<>) (VDouble v) (VDouble v') = VBool (v < v')

(</=>) :: Value -> Value -> Value 
(</=>) (VInt v) (VInt v') = VBool (v /= v')
(</=>) (VFloat v) (VFloat v') = VBool (v /= v')
(</=>) (VDouble v) (VDouble v') = VBool (v /= v')
(</=>) (VBool v) (VBool v') = VBool (v /= v')
(</=>) (VChar v) (VChar v') = VBool (v /= v')

not' :: Value -> Value 
not' (VBool v) = VBool (not v)

eval :: Exp -> State ([Env Value]) Value
eval (EVar v) = gets ((Map.! v) . head)
eval (ILit i) = return (VInt i)
eval (FLit i) = return (VFloat i)
eval (DLit i) = return (VDouble i)
eval (BLit i) = return (VBool i)
eval (CLit i) = return (VChar i)
eval (Plus e e') = liftM2 (<+>) (eval e) (eval e')
eval (Minus e e') = liftM2 (<->) (eval e) (eval e')
eval (Times e e') = liftM2 (<**>) (eval e) (eval e')
eval (Divided e e') = liftM2 (</>) (eval e) (eval e')
eval (And e e') =  liftM2 (<&&>) (eval e) (eval e')
eval (Or e e') =  liftM2 (<||>) (eval e) (eval e')
eval (Equal e e') = liftM2 (<==>) (eval e) (eval e')
eval (Gt e e') = liftM2 (<>>) (eval e) (eval e')
eval (Lt e e') = liftM2 (<<>) (eval e) (eval e')
eval (Ne e e') = liftM2 (</=>) (eval e) (eval e')
eval (Not e) =  liftM (not') (eval e)

type Prog = [Stmt]

type ProgState = (Map String Fundec, Env Value)

insertVar :: ProgState -> String -> Value -> ProgState
insertVar (f,m) s v = (f,Map.insert s v m)

execStmt :: Stmt -> State ([Env Value]) ()
execStmt (Assign v e) = do
                          i <- eval e
                          x <- gets
                          case (Map.member v (head x)) of
                             False -> fail
                             True -> modify (\(y:ys) -> (Map.update (\i' -> Just i) v y) : y : ys)
execStmt (Declare v e) = do
                           i <- eval e
                           modify (\(x:xs) -> (Map.insert v i x) : x : xs)
execStmt (If e xs (Just ys)) = do i <- eval e
                                  case i of 
                                     (VBool True) -> mapM_ execStmt xs
                                     (VBool False) -> mapM_ execStmt ys 

execStmt (If e xs Nothing) = do i <- eval e
                                case i of 
                                     (VBool True) -> mapM_ execStmt xs
                                     (VBool False) -> return () 
execStmt (While e xs) = whileM e xs
execStmt (CallFunc s xs) = do 
                             i <- mapM (eval) xs
                             modify (\(x:xs) -> (Map.insert s i x) : x : xs)
                             

whileM :: Exp -> [Stmt] -> State ([Env Value]) ()
whileM e  cmd = do r <- eval e
                   case r of 
                      (VBool True)  -> (mapM_ execStmt cmd) >> whileM e cmd   
                      (VBool False) -> return () 

test = Lt (EVar "x") (ILit 3)
body = Assign "x" (Plus (ILit 1) (EVar "x"))
adapt (VBool b) = b
initState = Map.fromList [("x",VInt 0)]

--exec :: [Stmt] -> [Env Value]
exec = execState (whileM test [body]) [initState]

--execStmt (While e xs) = do i <- eval e
  --                         until ()     mapM execStmt xs    

--execStmt (Declare s e) = do 
--                            i <- eval e

--execStmt (CallFunc s xs) =                




--Test
-- exec' :: Prog -> State ([Env Int]) ()
-- exec' = mapM_ execStmt

-- exec :: Prog -> [Env Int]
-- exec p = execState (exec' p) [Map.empty]

-- example :: Prog
-- example = [Assign "x" (Lit 0), Assign "y" (Plus (EVar "x") (Lit 1)), Assign "x" (Plus (EVar "y") (Lit 1))]
-- {-
-- x = 0
-- y = x + 1
-- x = y + 1
-- -}
-- cond :: Interp Int
-- cond s i = s == "x" && i < 0

-- prop :: LTL
-- prop = F (Atom "x")

