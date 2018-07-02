module DSL where

import AST
import Interpreter
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

type ASTSt = (Map String FuncDec,[Stmt],[Map String Type])

insCode :: ASTSt -> Stmt -> ASTSt
insCode  (f,stmts,ms) s = (f,stmts++[s],ms)

insCodeTy :: ASTSt -> Stmt -> String -> Type -> ASTSt
insCodeTy  (f,stmts,m:ms) stmt var t = (f,stmts++[stmt],(Map.insert var t m):ms)


verifyTy ::ASTSt -> Exp -> Type -> Bool
verifyTy (_,_,m:_) e t = either (\_ -> False) (\t' -> t==t' ) (checkExpr m e)

declare :: String -> Type -> Exp -> State ASTSt ()
declare s t e = do st@(_,_,m:_) <- get
                   case verifyTy st e t of 
                      False -> fail "[ERROR] Types don't match"
                      True ->  case (Map.lookup s m) of 
                                      Nothing -> put (insCodeTy st (Declare s e) s t)
                                      Just _  -> fail ("[ERROR] Attempting to declare an already declared variable " ++ s)

assign :: String -> Exp -> State ASTSt ()
assign s e = do st@(_,_,m:_) <- get
                maybe (fail (s ++ " not defined ")) 
                      (\t' -> if verifyTy st e t' 
                                 then put (insCode st (Assign s e)) 
                                 else fail (s ++ "[ERROR] Types don't match")) 
                      (Map.lookup s m)

ifThen :: Exp -> State ASTSt () -> State ASTSt ()
ifThen e thn = do st@(fc,_,ty) <- get
                  case verifyTy st e TyBool of
                    False -> fail "[ERROR] Types don't match"
                    True -> do (_,ys,_) <- return $ execState thn (fc,[],ty)
                               put (insCode st (If e ys Nothing))

ifThenElse :: Exp -> State ASTSt () -> State ASTSt () -> State ASTSt ()
ifThenElse e thn els = do st@(fc,_,ty) <- get
                          case verifyTy st e TyBool of
                            False -> fail "[ERROR] Types don't match"
                            True -> do (_,xs,_) <- return $ execState thn (fc,[],ty)
                                       (_,ys,_) <- return $ execState els (fc,[],ty)
                                       put (insCode st (If e xs (Just ys)))
                                
while :: Exp -> State ASTSt () -> State ASTSt ()
while e xs = do st@(fc,_,ty) <- get
                case verifyTy st e TyBool of
                   False -> fail "[ERROR] Types don't match"
                   True -> do (_,ys,_) <- return $ execState xs (fc,[],ty)
                              put (insCode st (While e ys))

delay :: Integer -> State ASTSt ()
delay time = do st <- get 
                put (insCode st (Delay time))

inputPin :: Int ->  State ASTSt ()
inputPin i = get >>= \st -> put (insCode st (DeclareInputPin i (0,255)))

inputPinRange :: Int -> (Int,Int) -> State ASTSt ()
inputPinRange i rng = get >>= \st -> put (insCode st (DeclareInputPin i rng))

readPin :: Int ->  String -> State ASTSt ()
readPin pin var = do st@(_,_,m:_) <- get
                     case verifyTy st (EVar var) TyInt of 
                        False -> fail "[ERROR] Types don't match"
                        True -> put (insCode st (ReadPin pin var))
 
-- writePin :: Int -> Exp -> State ASTSt ()
-- writePin pin e = do st@(_,_,m:_) <- get
--                     case verifyTy st e TyInt of 
--                         True  ->  put (insCode st (WritePin s e))
--                         False ->  fail "[ERROR] Types don't match"

-- callfunc :: String -> [Exp] -> State ASTSt
-- callfunc s xs = do (f,stmts,m) <- get
--                    case (Map.lookup s f) of
--                       Nothing -> fail ("[ERROR] Attempting to run a nonexistent function" ++ s)
--                       Just _ -> put (f, stmts ++ [CallFunc s xs],m)
                  
-- return :: Exp -> State ASTSt ()
-- return e = do (f,stmts,(m:ms)) <- get
--               let verif = checkExpr m e
--               case verif of
--                  Left s' -> error s'
--                  Right _ -> put (f, stmts ++ [Return e],m:ms)    

checkExpr :: Map String Type -> Exp -> Either String Type
checkExpr m (Plus e1 e2) = let t1 = checkExpr m e1
                               t2 = checkExpr m e2
                           in if t1 == t2 then t1 else Left "[ERROR] Plus operator must be applied to integer, float or double types" 
checkExpr m (Minus e1 e2) = let t1 = checkExpr m e1
                                t2 = checkExpr m e2
                            in if t1 == t2 then t1 else Left "[ERROR] Minus operator must be applied to integer, float or double types"
checkExpr m (Times e1 e2) = let t1 = checkExpr m e1
                                t2 = checkExpr m e2
                            in if t1 == t2 then t1 else Left "[ERROR] Times operator must be applied to integer, float or double types"
checkExpr m (Divided e1 e2) = let t1 = checkExpr m e1
                                  t2 = checkExpr m e2
                              in if t1 == t2 then t1 else Left "[ERROR] Divided operator must be applied to integer, float or double types"                            
checkExpr m (And e1 e2) = let t1 = checkExpr m e1
                              t2 = checkExpr m e2
                          in if t1 == t2 then t1 else Left "[ERROR] And operator must be applied to boolean types"
checkExpr m (Or e1 e2) = let t1 = checkExpr m e1
                             t2 = checkExpr m e2
                         in if t1 == t2 then  t1 else Left "[ERROR] Or operator must be applied to boolean types"                            
checkExpr m (Equal e1 e2) = let t1 = checkExpr m e1
                                t2 = checkExpr m e2
                            in if t1 == t2 then  Right TyBool else Left "[ERROR] Equal operator must be applied to integer, float, double or boolean types"
checkExpr m (Gt e1 e2) = let t1 = checkExpr m e1
                             t2 = checkExpr m e2
                         in if t1 == t2 then Right TyBool else Left "[ERROR] Greater than operator must be applied to integer, float or double types"
checkExpr m (Lt e1 e2) = let t1 = checkExpr m e1
                             t2 = checkExpr m e2
                         in if t1 == t2 then Right TyBool else Left "[ERROR] Less than operator must be applied to integer, float or double types"
checkExpr m (Ne e1 e2) = let t1 = checkExpr m e1
                             t2 = checkExpr m e2
                         in if t1 == t2 then Right TyBool else Left "[ERROR] Not equal operator must be applied to integer, float or double types"
checkExpr m (Mod e1 e2) = let t1 = checkExpr m e1
                              t2 = checkExpr m e2
                          in if t1 == t2 then t1 else Left "[ERROR] Mod operator must be applied to integer types"               
checkExpr m (ILit _) = Right TyInt
checkExpr m (FLit _) = Right TyFloat
checkExpr m (DLit _) = Right TyDouble
checkExpr m (BLit _) = Right TyBool
checkExpr m (CLit _) = Right TyChar
checkExpr m (EVar v) = case (Map.lookup v m) of
                          Just t -> Right t
                          Nothing -> Left "[ERROR] Nonexistent variable" 

var :: String -> Exp
var s = EVar s 

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

(.+.) :: Exp -> Exp -> Exp
e .+. e' = Plus e e'       

(.-.) :: Exp -> Exp -> Exp
e .-. e' = Minus e e'   

(.*.) :: Exp -> Exp -> Exp
e .*. e' = Times e e'   

(./.) :: Exp -> Exp -> Exp
e ./. e' = Divided e e'   

(.&&.) :: Exp -> Exp -> Exp
e .&&. e' = And e e'

(.||.) :: Exp -> Exp -> Exp
e .||. e' = Or e e'   

(.==.) :: Exp -> Exp -> Exp
e .==. e' = Equal e e'   

(.>.) :: Exp -> Exp -> Exp
e .>. e' = Gt e e' 

(.<.) :: Exp -> Exp -> Exp
e .<. e' = Lt e e'   

(.!=.) :: Exp -> Exp -> Exp
e .!=. e' = Ne e e'        

(.%.) :: Exp -> Exp -> Exp
e .%. e' = Mod e e'

neg :: Exp -> Exp
neg e = Not e

-- ecallfunc :: String -> [Exp] -> Exp
-- ecallfunc s e = ECallFunc s e  
