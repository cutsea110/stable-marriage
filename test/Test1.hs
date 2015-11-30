module Test1 where

import Prelude hiding (Ordering(..))
import StableMarriage.GaleShapley
import Data.Poset as PO (Ordering(..), Poset((<),(<=),(>=),(>)))
import Data.List (find)

-- | Active Boyes meets Passive Girls

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

instance Men Boy where
  type W Boy = Girl
  loves = ranking
  forget x = x { ranking = tail (ranking x) }

instance Women Boy Girl where
  acceptable w m = m `elem` selects w
  compare = mkCmp . selects
      where
        mkCmp :: [Boy] -> Boy -> Boy -> Ordering
        mkCmp bs b1 b2 = cmp mb1 mb2
            where
              cmp Nothing Nothing = NC
              cmp (Just _) Nothing = GT
              cmp Nothing (Just _) = LT
              cmp (Just (_, v)) (Just (_, w)) | v PO.> w = GT
                                              | v PO.< w = LT
                                              | v == w = EQ
                                              | otherwise = NC
              tpl = zip bs ([1..]::[Int])
              mb1 = find ((==b1).fst) tpl
              mb2 = find ((==b2).fst) tpl


test1 :: World Girl Boy
test1 = boys `meets` girls
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

test2 :: World Girl Boy
test2 = boys `meets` girls
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
