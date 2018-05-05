module CodeLibrary where

import AST
import DSL
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

ex1 :: State ASTSt ()
ex1 = do 
        declare "x" TyFloat (float 2)
        declare "y" TyFloat (float 1.0)
        assign "x" ((var "x") .+. (var "y"))