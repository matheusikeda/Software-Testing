module AST where

type Var = String

data Type = TyInt
          | TyChar 
          | TyFloat
          | TyDouble
          | TyBool
          deriving (Eq,Show,Ord)
          
data FuncDec = Func String Type [(Type,String)] [Stmt] deriving Show

data Stmt = Assign Var Exp
          | If Exp [Stmt] (Maybe [Stmt])
          | While Exp [Stmt]
          | Declare String Exp
          | CallFunc String [Exp]
          | Return Exp
          deriving (Eq,Show,Ord)

data Exp = EVar Var                                                                                   | ILit Int 
         | FLit Float
         | DLit Double
         | BLit Bool
         | CLit Char
         | Plus Exp Exp 
         | Minus Exp Exp
         | Times Exp Exp
         | Divided Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Equal Exp Exp
         | Gt Exp Exp -- Greater than
         | Lt Exp Exp -- Less than
         | Ne Exp Exp -- Not equal
         | Not Exp
         | ECallFunc String [Exp]
         deriving (Eq, Ord, Show)

data Value = VInt Int
            |VFloat Float
            |VDouble Double
            |VBool Bool
            |VChar Char
            deriving (Show)