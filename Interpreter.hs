module Interpreter where

import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map
import LTL (Env, Interp)
import AST

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

eval :: Exp -> State (ProgState) Value
eval (EVar v) = do 
                  x <- get
                  return  (snd $ liftM (Map.! v) x)
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
eval (ECallFunc s xs) = do
                          (f,m) <- get
                          case (Map.lookup s f) of
                              Just (Func s t ns cmds) -> do vals <- mapM eval xs 
                                                            put (f, exp2Env vals (map snd ns))
                                                            mapM_ execStmt cmds
                                                            (_,m') <- get
                                                            put (f,m)
                                                            case (Map.lookup "@return" m') of
                                                              Just v -> return v
                                                              Nothing -> fail "[ERROR] Function did not returned a value"
                                                                
                                                                 
                              Nothing -> fail ("Undefined function: " ++ s)

type Prog = [Stmt]
type ProgState = (Map String FuncDec, Env Value)

-- updateValState :: ProgState -> Value -> ProgState
-- updateValState (f,m) v = (f,Map.update v s m)
--                          where s = 

-- getValue :: ProgState -> Var -> Value
-- getValue (f,m) v = Map.lookup v m

-- getFunc :: ProgState -> String -> FuncDec
-- getFunc (f,m) s = Map.lookup s f

-- insertFunc :: ProgState ->  FuncDec -> ProgState 
-- insertFunc (f,m) func = (Map.insert func f)

insertVar :: ProgState -> String -> Value -> ProgState
insertVar (f,m) s v = (f,Map.insert s v m)

updateVar :: ProgState -> String -> Value -> ProgState
updateVar (f,m) s v = (f,Map.update (\_ -> Just v) s m)

execStmt :: Stmt -> State (ProgState) ([Env Value])
execStmt (Assign s e) = do
                          i <- eval e
                          x <- get
                          case (snd $ liftM (Map.member s) x) of
                             False -> fail "[ERROR] Undefined variable"
                             True -> modify (\st -> updateVar st s i) >> 
                                      (get >>= (\(_,r) -> return [r])) 
                                        
execStmt (Declare s e) = do
                           i <- eval e
                           modify (\(f,m) -> insertVar (f,m) s i) >> (get >>= (\(_,r) -> return [r]) )
execStmt (If e xs (Just ys)) = do 
                                 i <- eval e
                                 case i of 
                                    (VBool True) -> do lss <- mapM execStmt xs 
                                                       return $ (concat lss)
                                    (VBool False) -> do lss <- mapM execStmt ys 
                                                        return $ (concat lss)
execStmt (If e xs Nothing) = do 
                               i <- eval e
                               case i of 
                                    (VBool True) -> do lss <- mapM execStmt xs 
                                                       return $ (concat lss)
                                    (VBool False) -> get >>= (\(_,r) -> return [r])
execStmt (While e xs) = whileM e xs
-- execStmt (Return e) = do 
--                         i <- eval e
--                         x <- get
--                         modify (\(f,m) -> (f, Map.insert "@return" i m))
-- execStmt (CallFunc s xs) = do
--                              (f,m) <- get
--                              case (Map.lookup s f) of
--                                 Just (Func s t ns cmds) ->  do vals <- mapM eval xs
--                                                                put (f, exp2Env vals (map snd ns))
--                                                                mapM_ execStmt cmds
--                                                                put (f,m)
--                                 Nothing -> fail ("Undefined function: " ++ s)

whileM :: Exp -> [Stmt] -> State (ProgState) ([Env Value])
whileM e cmd = do r <- eval e
                  case r of 
                     (VBool True)  -> do lss <- mapM execStmt cmd 
                                         ls <- whileM e cmd
                                         return $ (concat lss) ++ ls 
                     (VBool False) -> get >>= (\(_,r) -> return [r])

exp2Env :: [Value] -> [String] -> Env Value
exp2Env vs ns = Map.fromList (zip ns vs)    

verifyParameters :: [Exp] -> [(Type,String)] -> Bool
verifyParameters [] [] = True -- o que retornar?
verifyParameters [] (y:ys) = False
verifyParameters (x:xs) [] = False
verifyParameters (x:xs) (y:ys) 
    | checkExpType x (fst y) = verifyParameters xs ys
    | otherwise = error "[ERROR] Incompatible number of parameters"

checkExpType :: Exp -> Type -> Bool
checkExpType (ILit i) TyInt = True
checkExpType (FLit i) TyFloat = True
checkExpType (DLit i) TyDouble = True
checkExpType (BLit i) TyBool = True
checkExpType (CLit i) TyChar = True 
checkExpType _ _ = False
                     
test = Lt (EVar "x") (ILit 3)
body = Assign "x" (Plus (ILit 1) (EVar "x"))
adapt (VBool b) = b
initState = Map.fromList [("x",VInt 0)]

--Test
-- exec' :: Prog -> State ([Env Int]) ()
-- exec' = mapM_ execStmt

-- exec :: Prog -> [Env Int]
-- exec p = execStagn "x" (Lit 0), Assign "y" ((EVar "x") :+: (Lit 1))
--          , Assign "x" ((EVar "y") :+: (Lit 1))]te (exec' p) [Map.empty]


func :: FuncDec 
func = Func "soma" TyInt [(TyInt,"x"),(TyInt,"y")] [Assign "x" (ILit 1) ,Return (Plus (EVar "x") (EVar "y"))]

