{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module StableMarriage.GaleShapley
       ( Men(..)
       , Women(..)
       , World
       , meets

       -- re-export
       , PO.Ordering(..)
       ) where

import Prelude hiding (Ordering(..), compare)
import Control.Arrow ((&&&))
import Data.List (sortOn, groupBy, splitAt)
import Data.Poset as PO (Ordering(..), sortBy')
import Data.Function (on)

class Men m where
  type W m :: *
  loves :: m -> [W m]
  forget :: m -> m

class (Ord w, Men m, w ~ W m) => Women m w where
  acceptable :: w -> m -> Bool
  compare :: w -> m -> m -> PO.Ordering
  limit :: w -> [m] -> Int
  limit _ _ = 1

type World w m = (Men m, Women m w, w ~ W m) => ([(w, [m])], [m])

marriage :: World w m -> World w m
marriage x = let x' = counter $ attack x
             in if stable x'
                then x'
                else marriage x'

stable :: (Men m, Women m w, w ~ W m) => World w m -> Bool
stable (cs, ms) = all resigned ms
    where
      resigned :: Men m => m -> Bool
      resigned = null . loves
      satisfy :: (Men m, Women m w, w ~ W m) => (w, [m]) -> Bool
      satisfy (w, ms) = limit w ms <= length ms

attack :: World w m -> World w m
attack (cs, ms) = (cs', ms')
    where
      cs' = join cs (propose ms)
      ms' = despair ms

propose :: (Ord w, Men m, Women m w, w ~ W m) => [m] -> [(w, [m])]
propose = gather . competes
    where
      competes :: (Men m, Women m w, w ~ W m, Ord w) => [m] -> [[(w, m)]]
      competes = groupBy ((==) `on` fst) . sortOn fst . concatMap next
          where
            next :: (Men m, Women m w, w ~ W m) => m -> [(w, m)]
            next m = let xs = loves m
                     in if null xs
                        then []
                        else [(head xs, m)]
gather :: (Men m, Women m w, w ~ W m) => [[(w, m)]] -> [(w, [m])]
gather = map sub
          where
            sub :: (Men m, Women m w, w ~ W m) => [(w, m)] -> (w, [m])
            sub cs@((w, m):_) = (w, map snd cs)

join :: (Men m, Women m w, w ~ W m) => [(w, [m])] -> [(w, [m])] -> [(w, [m])]
join cs xs = gather $ groupBy ((==) `on` fst) $ sortOn fst $ cs ++ xs
    where
      gather :: (Men m, Women m w, w ~ W m) => [[(w, [m])]] -> [(w, [m])]
      gather = map sub
          where
            sub :: (Men m, Women m w, w ~ W m) => [(w, [m])] -> (w, [m])
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


choice :: (Men m, Women m w, w ~ W m) => [(w, [m])] -> World w m
choice = gather . map judge
    where
      judge :: (Men m, Women m w, w ~ W m) => (w, [m]) -> ((w, [m]), [m])
      judge (w, ms) = let (n, p, cmp) = (limit w ms, acceptable w, compare w)
                          (cs, rs) = splitAt n $ sortBy' (p, cmp) ms
                          out = filter (not . p) ms
                      in ((w, cs), rs ++ out)
      gather :: (Men m, Women m w, w ~ W m) => [((w, [m]), [m])] -> World w m
      gather = map fst &&& concatMap snd

meets :: (Men m, Women m w, w ~ W m) => [m] -> [w] -> World w m
meets ms ws = marriage (zip ws (repeat []), ms)
