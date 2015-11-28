module StableMarriage.GaleShapley where

import Prelude hiding (Ordering(..), Ord(..))
import qualified Prelude as Org
import Control.Arrow ((&&&))
import Data.List (find, sortOn, groupBy, nub, (\\))
import Data.Maybe (isJust)
import Data.Poset (Ordering(..), sortBy', Poset(..))
import Data.Function (on)

type Name = String

data Male = M Name [Female]
instance Show Male where
  show (M n _) = n
instance Eq Male where
  (M n1 _) == (M n2 _) = n1 == n2
instance Org.Ord Male where
  M n1 _ <= M n2 _ = n1 <= n2

data Female = F Name ((Male -> Bool), (Male -> Male -> Ordering))
instance Show Female where
  show (F n _) = n
instance Eq Female where
  (F n1 _) == (F n2 _) = n1 == n2
instance Org.Ord Female where
  F n1 _ <= F n2 _ = n1 <= n2

alan, bill, charles, david :: Male
-- | Test Case 1
-- alan = M "Alan" [kathy, lucy, mary, nancy]
-- bill = M "Bill" [kathy, nancy, mary, lucy]
-- charles = M "Charles" [lucy, mary, kathy, nancy]
-- david = M "David" [lucy, mary, kathy, nancy]

-- | Test Case 2
alan = M "Alan" [kathy, lucy, mary, nancy]
bill = M "Bill" [nancy, kathy, mary, lucy]
charles = M "Charles" [lucy, mary, kathy, nancy]
david = M "David" [mary, lucy, kathy, nancy]

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

mkPred :: [Male] -> Male -> Bool
mkPred ms m = m `elem` ms

{- -- | Test Case 1
kathy, lucy, mary, nancy :: Female
kathy = F "Kathy" (mkPred ords, mkCmp ords)
    where
      ords = [charles, david, bill, alan]
lucy = F "Lucy" (mkPred ords, mkCmp ords)
    where
      ords = [david, charles, alan, bill]
mary = F "Mary" (mkPred ords, mkCmp ords)
    where
      ords = [david, alan, bill, charles]
nancy = F "Nancy" (mkPred ords, mkCmp ords)
    where
      ords = [charles, alan, bill, david]
-}

-- | Test Case 2
kathy, lucy, mary, nancy :: Female
kathy = F "Kathy" (mkPred ords, mkCmp ords)
    where
      ords = [charles, david, bill, alan]
lucy = F "Lucy" (mkPred ords, mkCmp ords)
    where
      ords = [david, alan, bill, charles]
mary = F "Mary" (mkPred ords, mkCmp ords)
    where
      ords = [david, alan, bill, charles]
nancy = F "Nancy" (mkPred ords, mkCmp ords)
    where
      ords = [charles, alan, david, bill]

guys :: [Male]
guys = [alan, bill, charles, david]
babies :: [Female]
babies = [kathy, lucy, mary, nancy]

type Couples = [(Female, [Male])]
type Males = [Male]
type Board = (Couples, Males)


marriage :: Board -> Board
marriage b = let b' = counter $ attack b
             in if stable b'
                then b'
                else marriage b'

stable :: Board -> Bool
stable (cs, ms) = all nochance ms || all keep cs
    where
      nochance :: Male -> Bool
      nochance (M _ fs) = null fs
      keep :: (Female, Males) -> Bool
      keep (_, ms) = not (null ms)

attack :: Board -> Board
attack (cs, ms) = (cs', ms')
    where
      cs' = join cs (propose ms)
      ms' = despair ms

propose :: Males -> Couples
propose = gather . competes
    where
      competes :: Males -> [[(Female, Male)]]
      competes = groupBy ((==) `on` fst) . sortOn fst . concatMap c'
          where
            c' (M _ []) = []
            c' m@(M _ (f:_)) = [(f, m)]
      gather :: [[(Female, Male)]] -> Couples
      gather = map sub
          where
            sub :: [(Female, Male)] -> (Female, [Male])
            sub cs@((f, m):_) = (f, map snd cs)

join :: Couples -> Couples -> Couples
join cs xs = gather $ groupBy ((==) `on` fst) $ sortOn fst $ cs ++ xs
    where
      gather :: [[(Female, [Male])]] -> [(Female, [Male])]
      gather = map sub
          where
            sub :: [(Female, [Male])] -> (Female, [Male])
            sub cs@((f, m):_) = (f, concatMap snd cs)

despair :: Males -> Males
despair = filter (\(M _ fs) -> null fs)

counter :: Board -> Board
counter (cs, ms) = (cs', ms'')
    where
      (cs', ms') = choice cs
      ms'' = ms ++ heartbreak ms'

type Keepies = Males
type Dumped = Males

choice :: Couples -> (Couples, Dumped)
choice = gather . map judge
    where
      judge :: (Female, Males) -> ((Female, Keepies), Dumped)
      judge (f@(F _ cmp), ms) = let ords = sortBy' cmp ms
                                in if null ords
                                   then ((f, []), [])
                                   else ((f, [head ords]), tail ords)
      gather :: [((Female, Keepies), Dumped)] -> (Couples, Dumped)
      gather = map fst &&& concatMap snd

heartbreak :: Dumped -> Males
heartbreak = map forget
    where
      forget :: Male -> Male
      forget (M n (_:fs)) = M n fs

initBoard :: Board
initBoard = (zip babies (repeat []), guys)
