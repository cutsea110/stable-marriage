{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module StableMarriage.GaleShapley
       ( Men(..)
       , Women(..)
       , reduce
       ) where

import Prelude hiding (Ordering(..), compare)
import Control.Arrow ((&&&))
import Data.List (sortOn, groupBy)
import Data.Poset as PO (Ordering(..), sortBy')
import Data.Function (on)

class Men m where
  type W m :: *
  loves :: m -> [W m]
  forget :: m -> m

class (Ord w, Men m, W m ~ w) => Women m w where
  acceptable :: w -> m -> Bool
  compare :: w -> m -> m -> PO.Ordering

type World w m = (Women m (W m), w ~ W m) => ([(w, [m])], [m])
type Couple w m = (Women m (W m), w ~ W m) => (w, [m])

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

propose :: (Ord (W m), Women m (W m)) => [m] -> [(W m, [m])]
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

join :: Women m (W m) => [(W m, [m])] -> [(W m, [m])] -> [(W m, [m])]
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


choice :: Women m (W m) => [(W m, [m])] -> ([(W m, [m])], [m])
choice = gather . map judge
    where
      judge :: (Men m, w ~ W m, Women m w) => (w, [m]) -> ((w, [m]), [m])
      judge (f, ms) = let ords = sortBy' (acceptable f, compare f) ms
                      in if null ords
                         then ((f, []), [])
                         else ((f, [head ords]), tail ords)
      gather :: (Men m, w ~ W m) => [((w, [m]), [m])] -> ([(w, [m])], [m])
      gather = map fst &&& concatMap snd

reduce :: Women m (W m) => [W m] -> [m] -> ([(W m, [m])], [m])
reduce ws ms = marriage (zip ws (repeat []), ms)
