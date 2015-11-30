{-# LANGUAGE RankNTypes #-}
module StableMarriage.GaleShapley where

import Prelude hiding (Ordering(..), compare)
import qualified Prelude
import Control.Arrow ((&&&))
import Data.List (find, sortOn, groupBy, nub, (\\))
import Data.Maybe (isJust)
import Data.Poset as PO (Ordering(..), sortBy', Poset((<),(<=),(>=),(>)))
import Data.Function (on)

class Men m where
  type W m :: *
  loves :: m -> [W m]
  forget :: m -> m

class (Ord w, Men m, W m ~ w) => Women m w where
  acceptable :: w -> m -> Bool
  compare :: w -> m -> m -> PO.Ordering

type World w m = (Men m, Women m w, W m ~ w) => ([(w, [m])], [m])
type Couple w m = (Men m, Women m w, W m ~ w) => (w, [m])

marriage :: World w m -> World w m
marriage x = let x' = counter $ attack x
              in if stable x'
                 then x'
                 else marriage x'
stable :: (Men m, Women m w) => World w m -> Bool
stable (cs, ms) = all nochance ms || all keep cs
    where
      nochance :: Men m => m -> Bool
      nochance = null . loves
      keep :: (w, [m]) -> Bool
      keep = not . null . snd

attack :: World w m -> World w m
attack (cs, ms) = (cs', ms')
    where
      cs' = join cs (propose ms)
      ms' = despair ms

propose :: (Men m, w ~ W m, Ord w) => [m] -> [(w, [m])]
propose = gather . competes
    where
      competes :: (Men m, w ~ W m, Ord w) => [m] -> [[(w, m)]]
      competes = groupBy ((==) `on` fst) . sortOn fst . concatMap next
          where
            next :: (Men m, w ~ W m) => m -> [(w, m)]
            next m = let xs = loves m
                     in if null xs
                        then []
                        else [(head xs, m)]
      gather :: (Men m, w ~ W m) => [[(w, m)]] -> [(w, [m])]
      gather = map sub
          where
            sub :: (Men m, w ~ W m) => [(w, m)] -> (w, [m])
            sub cs@((w, m):_) = (w, map snd cs)

join :: (Men m, w ~ W m, Ord w) => [(w, [m])] -> [(w, [m])] -> [(w, [m])]
join cs xs = gather $ groupBy ((==) `on` fst) $ sortOn fst $ cs ++ xs
    where
      gather :: (Men m, w ~ W m) => [[(w, [m])]] -> [(w, [m])]
      gather = map sub
          where
            sub :: (Men m, w ~ W m) => [(w, [m])] -> (w, [m])
            sub cs@((w, m):_) = (w, concatMap snd cs)

despair :: Men m => [m] -> [m]
despair = filter (null . loves)

counter :: World w m -> World w m
counter (cs, ms) = (cs', ms'')
    where
      (cs', ms') = choice cs
      ms'' = ms ++ heartbreak ms'

      heartbreak :: Men m => [m] -> [m]
      heartbreak = map forget


choice :: (Men m, w ~ W m, Women m w) => [(w, [m])] -> ([(w, [m])], [m])
choice = gather . map judge
    where
      judge :: (Men m, w ~ W m, Women m w) => (w, [m]) -> ((w, [m]), [m])
      judge (f, ms) = let ords = sortBy' (acceptable f, compare f) ms
                      in if null ords
                         then ((f, []), [])
                         else ((f, [head ords]), tail ords)
      gather :: (Men m, w ~ W m) => [((w, [m]), [m])] -> ([(w, [m])], [m])
      gather = map fst &&& concatMap snd

-- | Test

type Name = String
data Boy = B { boyName :: Name, ranking :: [Girl] } deriving (Show, Eq, Ord)
data Girl = G { girlName :: Name, selects :: [Boy] } deriving (Show, Eq, Ord)

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
