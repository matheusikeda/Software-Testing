module DSL where

import AST
import Interpreter
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map


type ASTSt = (Map String FuncDec,[Stmt],[Map String Type])

-- prog = do declare "x" TyInt (val 1)
--           declare "y" TyInt (val 1)
--           while (var "x" :<: 1000) 
--               (do (var "x") :<-: ()   

declare :: String -> Type -> Exp -> State ASTSt
declare s t e = do (f,stmts,m) <- get
                  case (Map.lookup s m) of
                     Nothing -> put (f, stmts ++ [Declare s e], Map.insert s t m)
                     Just _  -> fail ("[ERROR] Attempting to declare an already declared variable " ++ s)

assign :: String -> Exp -> State ASTSt
assign s e = do (f,stmts,m) <- get
               case (Map.lookup s m) of
                  Just t -> put (f, stmts ++ [Assign s e],m) --precisa adicionar o tipo?? Já esta no map??
                  Nothing -> fail ("[ERROR] Attempting to assign a nonexistent variable" ++ s)

ifelse :: Exp -> [Stmt] -> (Maybe [Stmt]) -> State ASTSt --como saber qual a verificacao??
ifelse e xs (Just ys) = do (f,stmts,m) <- get
                           put (f, stmts ++ [If e xs ys],m)
ifelse e xs Nothing = do (f,stmts,m) <- get
                         put (f, stmts ++ [If e xs Nothing],m)                        

while :: Exp -> [Stmt] -> State ASTSt -- como saber qual a verificacao??
while e xs = do (f,stmts,m) <- get
                put (f, stmts ++ [While e xs],m)

callfunc :: String -> [Exp] -> State ASTSt
callfunc s xs = do (f,stmts,m) <- get
                   case (Map.lookup s f) of
                      Nothing -> fail ("[ERROR] Attempting to run a nonexistent function" ++ s)
                      Just _ -> put (f, stmts ++ [CallFunc s xs],m)
                  
return :: Exp -> State ASTSt
return e = do (f,stmts,m) <- get
              put (f, stmts ++ [Return e],m)

-- var :: String -> State ASTSt
-- var v = do (f,stmts,m) <- get
--            case (Map.lookup v m) of
--                  Nothing -> put (f, stmts ++ [Declare v e], Map.insert m (v,t))
--                  Just _  -> fail ("Attempting to declare an already declared variable " ++ v)            

-- usar State Exp??
var :: String -> State Exp
var s = do put (Evar s) 

int :: Int -> State Exp
int i = do put (ILit i)

float :: Float -> State Exp
float f = do put (FLit f)

double :: Double -> State Exp
double d = do put (DLit d)

bool :: Bool -> State Exp
bool b = do put (BLit b)

char :: Char -> State Exp
char c = do put (CLit c)

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
