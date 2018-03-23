module DSL where

import AST
import Interpreter
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

type ASTSt = (Map String FuncDec, [Stmt],[Map String Type])


declare :: String -> Exp -> Stmt
declare s t e = Delcare s e

var :: String -> State Exp
var v = do (stmts,m) <- get
           case (Map.lookup v m) of
                 Nothing -> put (stmts ++ [Declare v e], Map.insert m (v,t))
                 Just _  -> fail ("Attempting to declare an already declared variable " ++ v)
                 
                 
                 
            
prog = do declare "x" TyInt (val 1)
          delcare "y" TyInt (val 1)
          while (var "x" :<: 1000) 
              (do (var "x") :<-: ()   
