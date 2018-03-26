module DSL where

import AST
import Interpreter
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

type ASTSt = (Map String FuncDec, [Stmt],[Map String Type])


declare :: String -> Exp -> Stmt
declare s e = Declare s e

assign :: String -> Exp -> Stmt
assign s e = Assign s e

ifelse :: Exp -> [Stmt] -> (Maybe [Stmt]) -> Stmt
ifelse e xs ys = If e xs ys

while :: Exp -> [Stmt] -> Stmt
while e xs = While e xs

callfunc :: String -> [Exp] -> Stmt
callfunc s xs = CallFunc s xs

return :: Exp -> Stmt
return e = Return e

var :: String -> State ASTSt
var v = do (f,stmts,m) <- get
           case (Map.lookup v m) of
                 Nothing -> put (f, stmts ++ [Declare v e], Map.insert m (v,t))
                 Just _  -> fail ("Attempting to declare an already declared variable " ++ v)
                 
-- int :: Int -> State ASTSt
-- int i = do (f,stmts,m) <- get
-- 		   put (f,stmts ++ [])                
                 
plus :: Exp -> Exp -> State Exp
plus e e' = do put (Plus e e')       

minus :: Exp -> Exp -> State Exp
minus e e' = do put (Minus e e')   

times :: Exp -> Exp -> State Exp
times e e' = do put (Times e e')   

divided :: Exp -> Exp -> State Exp
divided e e' = do put (Divided e e')   

and :: Exp -> Exp -> State Exp
and e e' = do put (And e e')   

or :: Exp -> Exp -> State Exp
or e e' = do put (Or e e')   

equal :: Exp -> Exp -> State Exp
equal e e' = do put (Equal e e')   

gt :: Exp -> Exp -> State Exp
gt e e' = do put (Gt e e')   

lt :: Exp -> Exp -> State Exp
lt e e' = do put (Lt e e')   

ne :: Exp -> Exp -> State Exp
ne e e' = do put (Ne e e')        

not :: Exp -> State Exp
not e = do put (Not e e')

ecallfunc :: String -> [Exp] -> State Exp
ecallfunc s e = do put (ECallFunc s e)  
            
prog = do declare "x" TyInt (val 1)
          declare "y" TyInt (val 1)
          while (var "x" :<: 1000) 
              (do (var "x") :<-: ()   
