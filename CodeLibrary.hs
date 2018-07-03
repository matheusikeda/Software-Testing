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
        

ex5 :: State ASTSt ()
ex5 = do 
         declare "y" TyInt ((int 2) .+. (int 3))
         assign "y" (int 5)
         assign "y" (int 6)
                       

                              
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

--Semaforos
ex4 :: State ASTSt ()
ex4 = do
        declare "pin5" TyInt (int 0) -- vermelho 1
        declare "pin6" TyInt (int 0) -- amarelo 1
        declare "pin7" TyInt (int 0) -- verde 1
        declare "pin8" TyInt (int 0) -- vermelh 2o
        declare "pin9" TyInt (int 0) -- amarelo 2
        declare "pin10" TyInt (int 0) -- verde 2
        declare "ligado" TyBool (bool True)
        while ((var "ligado") .==. (bool True)) 
              (do 
                 assign "pin5" (int 0)
                 assign "pin7" (int 5)
                 assign "pin8" (int 5)
                 delay(40)
                 assign "pin7" (int 0)
                 assign "pin6" (int 5)
                 delay(20)
                 assign "pin6" (int 0)
                 assign "pin5" (int 5)
                 assign "pin8" (int 0)                          
                 assign "pin10" (int 5)
                 delay(40)
                 assign "pin10" (int 0)
                 assign "pin9" (int 5)
                 delay(20)
                 assign "pin9" (int 0))

ex6 :: State ASTSt ()
ex6 = do 
         inputPin 1
         declare "y" TyInt (int 0)
         readPin 1 "y"
         writePin 2 (int 5)
        