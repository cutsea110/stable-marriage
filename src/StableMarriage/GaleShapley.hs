module StableMarriage.GaleShapley where

import Prelude hiding (Ordering(..), Ord(..))
import Data.List (find)
import Data.Poset

type Name = String

data Male = M Name [Female]
instance Show Male where
  show (M n _) = n
instance Eq Male where
  (M n1 _) == (M n2 _) = n1 == n2

data Female = F Name (Male -> Male -> Ordering)
instance Show Female where
  show (F n _) = n
instance Eq Female where
  (F n1 _) == (F n2 _) = n1 == n2

alan, bill, charles, david :: Male
alan = M "Alan" [kathy, mary, nancy]
bill = M "Bill" [lucy, kathy]
charles = M "Charles" [lucy, mary]
david = M "David" [mary, nancy, lucy]

mkCmp :: [Male] -> Male -> Male -> Ordering
mkCmp ms x y = comp mx my
    where
      comp Nothing Nothing = NC
      comp (Just _) Nothing = GT
      comp Nothing (Just _) = LT
      comp (Just (l, v)) (Just (r, w)) | v > w = GT
                                       | v < w = LT
                                       | v == w = EQ
                                       | otherwise = NC
      tpl = zip ms ([1..] :: [Int])
      mx = find ((==x).fst) tpl
      my = find ((==y).fst) tpl

kathy, lucy, mary, nancy :: Female
kathy = F "Kathy" (mkCmp ords)
    where
      ords = [bill, alan]
lucy = F "Lucy" (mkCmp ords)
    where
      ords = [david, bill, charles]
mary = F "Mary" (mkCmp ords)
    where
      ords = [alan, charles, david]
nancy = F "Nancy" (mkCmp ords)
    where
      ords = [david, alan]

propose :: Male -> (Male, Maybe Female)
propose m@(M _ []) = (m, Nothing)
propose (M n (f:fs)) = (M n fs, Just f)

keep :: Female -> [Male] -> (Female, Maybe Male)
keep f [] = (f, Nothing)
keep f (m:ms) = (f, f `choice` ms)

choice :: Female -> [Male] -> Maybe Male
choice _ [] = Nothing
choice f ms = undefined

marriage :: ([(Male, Female)], [Male], [Female]) -> ([(Male, Female)], [Male], [Female])
marriage res@(_, _, []) = res
marriage res@(_, [], _) = res
marriage (couples, males, females) = undefined
