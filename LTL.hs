module LTL where
  
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

data LTL = Atom String
         | TT | FF
         | LTL :&: LTL
         | LTL :|: LTL
         | Not LTL
         | G LTL
         | F LTL
         | X LTL
         | U LTL LTL
         | W LTL LTL
         | R LTL LTL
         deriving (Eq, Ord, Show)

type Env a = Map String a
type Interp a = String -> a -> Bool

lookupVal :: Interp a -> String -> [Env a] -> Bool
lookupVal interp s (x : xs) = case Map.lookup s x of
                                 Just y  -> interp s y
                                 Nothing -> False

checkLTL :: Interp a -> LTL -> [Env a] -> Bool
checkLTL interp TT ss = True
checkLTL interp FF ss = False
checkLTL interp (Atom s) ss = lookupVal interp s ss
checkLTL interp (l :&: r) ss = (checkLTL interp l ss) && (checkLTL interp r ss)
checkLTL interp (l :|: r) ss = (checkLTL interp l ss) || (checkLTL interp r ss)
checkLTL interp (Not l) ss = not (checkLTL interp l ss)
checkLTL interp (G l) ss = all (checkLTL interp l . (:[])) ss
checkLTL interp (F l ) ss = or (map (checkLTL interp l . (:[])) ss)
checkLTL interp (X l) ss = checkLTL interp l (tail ss)
checkLTL interp (U l r) ss = let
                               ss' = dropWhile (checkLTL interp l . (: [])) ss
                             in checkLTL interp r ss'
checkLTL interp (W l r) ss = checkLTL interp ((U l r) :|: (G l)) ss
checkLTL interp (R l r) ss = checkLTL interp (W r (l :&: r)) ss

-- checkLTLZ :: Interp a -> LTL -> (Bool,[Env a],[Env a]) -> (Bool,[Env a],[Env a]) 
-- checkLTLZ interp TT (_,p,f) = (True,p,f)
-- checkLTLZ interp FF (_,p,f) = (False,p,f)
-- checkLTLZ interp (Atom s) (_,p,f) = (lookupVal interp s f,p,f)
-- checkLTLZ interp (l :&: r) (_,p,f) = ((checkLTLZ interp l f) && (checkLTLZ interp r f),p,f)
-- checkLTLZ interp (l :|: r) (_,p,f) = ((checkLTLZ interp l f) || (checkLTLZ interp r f),p,f)
-- checkLTLZ interp (Not l) (_,p,f) = (not (checkLTLZ interp l f),p,f)
-- checkLTLZ interp (X l) (_,p,x:xs) = checkLTLZ interp l (True,x:p,xs)

