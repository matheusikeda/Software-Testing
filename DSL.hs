module DSL where

import AST
import Interpreter
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map


type ASTSt = (Map String FuncDec,[Stmt],[Map String Type])

-- prog = do declare "x" TyInt (val 1)
--           declare "y" TyInt (val 1)
             if(
                 exp,
                 (do ... ) 
             )
--           while (var "x" :<: (ival 1000)) 
--               (do (var "x") :<-: ()   

declare :: String -> Type -> Exp -> State ASTSt
declare s t e = do (f,stmts,m) <- get
                   let verif = checkExpr m e
                   case (Map.lookup s m) of 
                      Nothing -> put (f, stmts ++ [Declare s verif], Map.insert s t m)
                      Just _  -> fail ("[ERROR] Attempting to declare an already declared variable " ++ s)

assign :: String -> Exp -> State ASTSt
assign s e = do (f,stmts,m) <- get
                let verif = checkExpr m e
                case (Map.lookup s m) of
                   Just t -> put (f, stmts ++ [Assign s verif],m)
                   Nothing -> fail ("[ERROR] Attempting to assign a nonexistent variable" ++ s)

if' :: Exp -> [Stmt] -> State ASTSt
if' e xs = do (f,stmts,m) <- get
              let verif = checkExpr m e
              put (f, stmts ++ [If verif xs Nothing],m)

else' :: [Stmt]              

while :: Exp -> [Stmt] -> State ASTSt
while e xs = do (f,stmts,m) <- get
                let verif = checkExpr m e
                put (f, stmts ++ [While verif xs],m)

callfunc :: String -> [Exp] -> State ASTSt
callfunc s xs = do (f,stmts,m) <- get
                   case (Map.lookup s f) of
                      Nothing -> fail ("[ERROR] Attempting to run a nonexistent function" ++ s)
                      Just _ -> put (f, stmts ++ [CallFunc s xs],m)
                  
return :: Exp -> State ASTSt
return e = do (f,stmts,m) <- get
              let verif = checkExpr m e
              put (f, stmts ++ [Return e],m)    

checkExpr :: Map String Type -> Exp -> Either String Type
checkExpr m (Plus e1 e2) = let t1 = checkExpr e1
                               t2 = checkExpr e2
                           in if t1 == t2 then (Right t1) else Left "[ERROR] Plus operator must be applied to integer, float or double types" 
checkExpr m (Minus e1 e2) = let t1 = checkExpr e1
                                t2 = checkExpr e2
                            in if t1 == t2 then (Right t1) else Left "[ERROR] Minus operator must be applied to integer, float or double types"
checkExpr m (Times e1 e2) = let t1 = checkExpr e1
                                t2 = checkExpr e2
                            in if t1 == t2 then (Right t1) else Left "[ERROR] Times operator must be applied to integer, float or double types"
checkExpr m (Divided e1 e2) = let t1 = checkExpr e1
                                t2 = checkExpr e2
                              in if t1 == t2 then (Right t1) else Left "[ERROR] Divided operator must be applied to integer, float or double types"                            
checkExpr m (And e1 e2) = let t1 = checkExpr e1
                                t2 = checkExpr e2
                          in if t1 == t2 then (Right t1) else Left "[ERROR] And operator must be applied to boolean types"
checkExpr m (Or e1 e2) = let t1 = checkExpr e1
                              t2 = checkExpr e2
                         in if t1 == t2 then (Right t1) else Left "[ERROR] Or operator must be applied to boolean types"                            
checkExpr m (Equal e1 e2) = let t1 = checkExpr e1
                              t2 = checkExpr e2
                            in if t1 == t2 then (Right t1) else Left "[ERROR] Equal operator must be applied to integer, float, double or boolean types"
checkExpr m (Gt e1 e2) = let t1 = checkExpr e1
                             t2 = checkExpr e2
                         in if t1 == t2 then (Right t1) else Left "[ERROR] Greater than operator must be applied to integer, float or double types"
checkExpr m (Lt e1 e2) = let t1 = checkExpr e1
                             t2 = checkExpr e2
                         in if t1 == t2 then (Right t1) else Left "[ERROR] Less than operator must be applied to integer, float or double types"
checkExpr m (Ne e1 e2) = let t1 = checkExpr e1
                             t2 = checkExpr e2
                         in if t1 == t2 then (Right t1) else Left "[ERROR] Not equal operator must be applied to integer, float or double types"
checkExpr m (ILit _) = Right TyInt
checkExpr m (FLit _) = Right TyFloat
checkExpr m (DLit _) = Right TyDouble
checkExpr m (BLit _) = Right TyBool
checkExpr m (CLit _) = Right TyChar
checkExpr m (EVar v) = case (Map.lookup v m) of
                          Just t -> Right t
                          Nothing -> Left "[ERROR] Nonexistent variable" 

var :: String -> Exp
var s = Evar s 

int :: Int -> Exp
int i = ILit i

float :: Float -> Exp
float f = FLit f

double :: Double -> Exp
double d = DLit d

bool :: Bool -> Exp
bool b = BLit b

char :: Char -> Exp
char c = CLit c

(:+:) :: Exp -> Exp -> Exp
e :+: e' = do put (Plus e e')       

(:-:) :: Exp -> Exp -> Exp
e :-: e' = Minus e e'   

(:*:) :: Exp -> Exp -> Exp
e :*: e' = Times e e'   

(:/:) :: Exp -> Exp -> Exp
e :/: e' = Divided e e'   

(:&&:) :: Exp -> Exp -> Exp
e :&&: e' = And e e'

(:||:) :: Exp -> Exp -> Exp
e :||: e' = Or e e'   

(:==:) :: Exp -> Exp -> Exp
e :==: e' = Equal e e'   

(:>:) :: Exp -> Exp -> Exp
e :>: e' = Gt e e' 

(:<:) :: Exp -> Exp -> Exp
e :<: e' = Lt e e'   

(:!=:) :: Exp -> Exp -> Exp
e :!=: e' = Ne e e'        

(:!:) :: Exp -> Exp
:!: e = Not e e'

ecallfunc :: String -> [Exp] -> Exp
ecallfunc s e = ECallFunc s e  
