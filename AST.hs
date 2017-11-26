module AST where

type Var = String

data Type = TYInt
          | TyChar 
          | TyFloat
          | TyDouble
          | TyBool
          deriving (Eq,Show,Ord)
          
data FuncDec = Func String Type [(Type,String)] [Stmt]

data Stmt = Assign Var Exp
          | If Exp [Stmt] (Maybe [Stmt])
          | While Exp [Stmt]
          | Declare String Exp
          | CallFunc String [Exp]
          deriving (Eq,Show,Ord)

data Exp = EVar Var 
         | ILit Int 
         | FLit Float
         | DLit Double
         | BLit Bool
         | CLit Char
         | Plus Exp Exp 
         | Minus Exp Exp
         | Times Exp Exp
         | Divided Exp Exp
         | Not Exp
         | And Exp Exp
         | Or Exp Exp
         | Equal Exp Exp
         | Gt Exp Exp -- Greater than
         | Lt Exp Exp -- Less than
         | Ne Exp Exp -- Not equal
         deriving (Eq, Ord, Show)

data Value = VInt Int
            |VFloat Float
            |VDouble Double
            |VBool Bool
            |VChar Char
            deriving (Show)