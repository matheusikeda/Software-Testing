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
-- type Interp a = String -> a -> Bool
type Interp a = String -> Env a -> Bool

lookupVal :: Interp a -> String -> [Env a] -> Bool
lookupVal p s (x : xs) = p s x 

-- checkLTL :: Interp a -> LTL -> [Env a] -> Bool
-- checkLTL interp TT ss = True
-- checkLTL interp FF ss = False
-- checkLTL interp (Atom s) ss = lookupVal interp s ss
-- checkLTL interp (l :&: r) ss = (checkLTL interp l ss) && (checkLTL interp r ss)
-- checkLTL interp (l :|: r) ss = (checkLTL interp l ss) || (checkLTL interp r ss)
-- checkLTL interp (LTL.Not l) ss = not (checkLTL interp l ss)
-- checkLTL interp (G l) ss = all (checkLTL interp l . (:[])) ss
-- checkLTL interp (F l ) ss = or (map (checkLTL interp l . (:[])) ss)
-- checkLTL interp (X l) ss = checkLTL interp l (tail ss)
-- checkLTL interp (U l r) ss = let
--                                ss' = dropWhile (checkLTL interp l . (: [])) ss
--                              in checkLTL interp r ss'
-- checkLTL interp (W l r) ss = checkLTL interp ((U l r) :|: (G l)) ss
-- checkLTL interp (R l r) ss = checkLTL interp (W r (l :&: r)) ss

mkMaybe :: Bool -> a -> Maybe a
mkMaybe True x = Just x
mkMaybe False _ = Nothing 

(|&>) :: (Bool, Maybe (Env a)) ->  (Bool, Maybe (Env a)) -> (Bool, Maybe (Env a))
(|&>) (True,_)   (True, _) = (True, Nothing)
(|&>) (False,r)  _         = (False,r)
(|&>) _          (False,r) = (False,r)

(|+>) :: (Bool, Maybe (Env a)) ->  (Bool, Maybe (Env a)) -> (Bool, Maybe (Env a))
(|+>) (False,r)   (False, _) = (False, r)
(|+>) _           _          = (True,Nothing)

(|!>) :: (Bool, Maybe (Env a)) ->  (Bool, Maybe (Env a))
(|!>) (True,r) = (False,r)
(|!>) (False,_) = (True,Nothing)

lstCheck :: [(Bool,Maybe (Env a))] -> (Bool,Maybe (Env a))
lstCheck xs = case dropWhile fst xs of
                  [] -> (True,Nothing)
                  (x:_) -> x                                

orCheck :: [(Bool,Maybe (Env a))] -> (Bool, Maybe (Env a))
orCheck xs = case or (map fst xs) of
                  True -> (True, Nothing)
                  False -> (False, Just Map.empty)

checkLTLZ :: Interp a -> LTL -> [Env a] -> (Bool,Maybe (Env a)) 
checkLTLZ interp TT es             = (True,Nothing)
checkLTLZ interp FF es             = (False,Just $ head es)
checkLTLZ interp (Atom s)  es      = let r = lookupVal interp s es in (r,mkMaybe r  (head es))
checkLTLZ interp (l :&: r) es      = (checkLTLZ interp l es) |&> (checkLTLZ interp r es)
checkLTLZ interp (l :|: r) es      = (checkLTLZ interp l es)  |+> (checkLTLZ interp r es)
checkLTLZ interp (Not l)   es      = (|!>) (checkLTLZ interp l es)
checkLTLZ interp (X l)    (_:xs)   = checkLTLZ interp l xs
checkLTLZ interp (G l) es          = lstCheck $ map (checkLTLZ interp l .(:[])) es 
checkLTLZ interp (F l) es          = orCheck  $ map (checkLTLZ interp l .(:[])) es
checkLTL interp (U l r) ss            = let ss' = dropWhile (fst.(checkLTLZ interp l . (: []))) ss 
                                        in checkLTL interp r ss'
checkLTL interp (W l r) ss            = checkLTLZ interp ((U l r) :|: (G l)) ss
checkLTL interp (R l r) ss            = checkLTLZ interp (W r (l :&: r)) ss
