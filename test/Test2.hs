module Test2 where

import Prelude hiding (Ordering(..))
import StableMarriage.GaleShapley
import Data.List (find)

-- | Active Girls meets Passive Boys

type Name = String
data Boy = B { boyName :: Name, ranking :: [Girl] }
data Girl = G { girlName :: Name, selects :: [Boy] }

instance Show Boy where
  show (B n _) = n
instance Eq Boy where
  B n1 _ == B n2 _ = n1 == n2
instance Ord Boy where
  B n1 _ <= B n2 _ = n1 Prelude.<= n2

instance Show Girl where
  show (G n _) = n
instance Eq Girl where
  G n1 _ == G n2 _ = n1 == n2
instance Ord Girl where
  G n1 _ <= G n2 _ = n1 Prelude.<= n2

instance Men Girl where
  type W Girl = Boy
  loves = selects
  forget x = x { selects = tail (selects x) }

instance Women Girl Boy where
  acceptable w m = m `elem` ranking w
  compare = mkCmp . ranking
      where
        mkCmp :: [Girl] -> Girl -> Girl -> Ordering
        mkCmp bs b1 b2 = cmp mb1 mb2
            where
              cmp Nothing Nothing = NC
              cmp (Just _) Nothing = GT
              cmp Nothing (Just _) = LT
              cmp (Just (_, v)) (Just (_, w)) | v > w = GT
                                              | v < w = LT
                                              | v == w = EQ
                                              | otherwise = NC
              tpl = zip bs ([1..]::[Int])
              mb1 = find ((==b1).fst) tpl
              mb2 = find ((==b2).fst) tpl


test1 :: World Boy Girl
test1 = girls `meets` boys
    where
      mA, mB, mC, mD :: Boy
      mA = B "A" [wa, wb, wc, wd]
      mB = B "B" [wa, wd, wc, wb]
      mC = B "C" [wb, wc, wa, wd]
      mD = B "D" [wb, wc, wa, wd]
  
      wa, wb, wc, wd :: Girl
      wa = G "a" [mC, mD, mB, mA]
      wb = G "b" [mD, mC, mA, mB]
      wc = G "c" [mD, mA, mB, mC]
      wd = G "d" [mC, mA, mB, mD]
      
      girls = [wa, wb, wc, wd]
      boys = [mA, mB, mC, mD]

test2 :: World Boy Girl
test2 = girls `meets` boys
    where
      mA, mB, mC, mD :: Boy
      mA = B "A" [wa, wb, wc, wd]
      mB = B "B" [wd, wa, wc, wb]
      mC = B "C" [wb, wc, wa, wd]
      mD = B "D" [wc, wb, wa, wd]
  
      wa, wb, wc, wd :: Girl
      wa = G "a" [mC, mD, mB, mA]
      wb = G "b" [mD, mA, mB, mC]
      wc = G "c" [mD, mA, mB, mC]
      wd = G "d" [mC, mA, mD, mB]
      
      girls = [wa, wb, wc, wd]
      boys = [mA, mB, mC, mD]
