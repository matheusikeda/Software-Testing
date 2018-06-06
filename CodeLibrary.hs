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
        ifThen ((var "x") .==. (float 2))
               (do assign "x" ((var "x") .+. (var "y")))
        


ex2 :: State ASTSt ()
ex2 = do
        declare "ledPin" TyInt (int 13)
        declare "inPin" TyInt (int 2)
        declare "val" TyInt (int 0)
        declare "digitalRead" TyInt (int 5)
        declare "digitalWrite" TyInt (int 0)
        assign "val" (var "digitalRead")
        ifThenElse ((var "val") .==. (int 5)) 
                   (do assign "digitalWrite" (int 0))
                   (do assign "digitalWrite" (int 5))

ex3 :: State ASTSt ()
ex3 = do        
        declare "buttonPin" TyInt (int 2)
        declare "ledPin" TyInt (int 13)
        declare "buttonPushCounter" TyInt (int 0)
        declare "buttonState" TyInt (int 0)
        declare "lastButtonState" TyInt (int 0)
        declare "digitalRead" TyInt (int 5)
        declare "digitalWrite" TyInt (int 0)
        assign "buttonState" (var "digitalRead")
        ifThen ((var "buttonState") .!=. (var "lastButtonState"))
               (do ifThen ((var "buttonState") .==. (int 5)) 
                          (do assign "buttonPushCounter" ((var "buttonPushCounter") .+. (int 1))))
        ifThenElse (((var "buttonPushCounter") .%. (int 4)) .==. (int 0))
                   (do assign "digitalWrite" (int 5))
                   (do assign "digitalWrite" (int 0))                        