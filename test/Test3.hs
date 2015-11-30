module Test3 where

import Prelude hiding (Ordering(..))
import StableMarriage.GaleShapley
import Data.List (find)

type Name = String
data Applicant = A { name :: Name, ranking :: [School] }
data School = S { school :: Name, selects :: [Applicant] , capacity :: Int }

instance Show Applicant where
  show (A n _) = n
instance Eq Applicant where
  A n1 _ == A n2 _ = n1 == n2
instance Ord Applicant where
  A n1 _ <= A n2 _ = n1 Prelude.<= n2

instance Show School where
  show (S n _ _) = n
instance Eq School where
  S n1 _ _ == S n2 _ _ = n1 == n2
instance Ord School where
  S n1 _ _ <= S n2 _ _ = n1 Prelude.<= n2

instance Men Applicant where
  type W Applicant = School
  loves = ranking
  forget x = x { ranking = tail (ranking x) }

instance Women Applicant School where
  acceptable w m = m `elem` selects w
  compare = mkCmp . selects
      where
        mkCmp :: [Applicant] -> Applicant -> Applicant -> Ordering
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
  limit = const . capacity

test1 :: World School Applicant
test1 = applicants `meets` schools

aa, ab, ac, ad, ae :: Applicant
aa = A "a" [sA, sB]
ab = A "b" [sA, sB]
ac = A "c" [sA, sB]
ad = A "d" [sB, sA]
ae = A "e" [sB, sA]
applicants = [aa, ab, ac, ad, ae]
  
sA, sB :: School
sA = S "A" [ad, ab, aa, ae, ac] 2
sB = S "B" [aa, ab, ac, ae, ad] 2
schools = [sA, sB]
