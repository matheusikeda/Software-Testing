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
        declare "ligado" TyBool (bool True)
        while ((var "ligado") .==. (bool True)) 
              (do 
                 writePin 5 (int 0)
                 writePin 7 (int 5)
                 writePin 8 (int 5)
                 delay(40)
                 writePin 7 (int 0)
                 writePin 6 (int 5)
                 delay(20)
                 writePin 6 (int 0)
                 writePin 5 (int 5)
                 writePin 8 (int 0)                          
                 writePin 10 (int 5)
                 delay(40)
                 writePin 10 (int 0)
                 writePin 9 (int 5)
                 delay(20)
                 writePin 9 (int 0)
                 writePin 8 (int 5))

ex4' :: State ASTSt ()
ex4' = do
        while (bool True) 
              (do 
                 writePin 5 (int 0)
                 writePin 7 (int 5)
                 writePin 9 (int 5)
                 delay(40)
                 writePin 7 (int 0)
                 writePin 6 (int 5)
                 delay(20)
                 writePin 6 (int 0)
                 writePin 5 (int 5)
                 writePin 8 (int 0)                          
                 writePin 10 (int 5)
                 delay(40)
                 writePin 10 (int 0)
                 writePin 9 (int 5)
                 delay(20)
                 writePin 9 (int 0))

ex6 :: State ASTSt ()
ex6 = do 
         inputPin 1
         declare "y" TyInt (int 0)
         readPin 1 "y"
         writePin 2 (int 5)
        
exemplo :: State ASTSt ()
exemplo = do
            declare "a" TyInt (int 0)
            declare "b" TyInt (int 0)
            while ((var "a") .<. (int 5))
                  (do 
                     assign "a" ((var "a") .+. (int 1))
                     assign "b" ((var "a") .*. (int 2))
                  )         

temp :: State ASTSt ()                  
temp = do
         declare "temperatura" TyInt (int 0)
         inputPin 1
         while (bool True) 
               (do 
                  readPin 1 "temperatura" 
                  writePin 5 (var "temperatura"))
                  