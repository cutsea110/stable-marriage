-- | Partially ordered data types. The standard 'Prelude.Ord' class is for
-- total orders and therefore not suitable for floating point.
-- However, we can still define meaningful 'max' and 'sort' functions for these types.
--
-- We define our own 'Ord' class which is intended as a replacement for
-- 'Prelude.Ord'. However, in order to take advantage of existing libraries
-- which use 'Prelude.Ord', we make every instance of 'Ord' an instance of
-- 'Prelude.Ord'. This is done using the OVERLAPS and UndecidableInstances
-- extensions -- it remains to be seen if problems occur as a result of this.
module Data.Poset
       ( Poset(..), Sortable(..), Ordering(..), Ord,
         module Data.Poset
       ) where

import qualified Prelude
import Prelude hiding (Ord(..), Ordering(..))
import qualified Data.List as List
import Data.Poset.Instances
import Data.Poset.Internal

import Data.Function
import Data.Monoid

instance Poset a => Poset (Maybe a) where
  Just x  <= Just y  = x <= y
  Nothing <= _       = True
  _       <= _       = False

instance Poset a => Poset [a] where
  compare = (mconcat .) . zipWith compare

-- | Sort a list using the default comparison function.
sort :: Sortable a => [a] -> [a]
sort = sortBy compare

-- | Sort a list using (isOrder, compare).
sortBy' :: ((a -> Bool), (a -> a -> Ordering)) -> [a] -> [a]
sortBy' (p, f) = List.sortBy ((totalOrder.).f) . filter p

-- | Apply a function to values before comparing.
comparing :: Poset b => (a -> b) -> a -> a -> Ordering
comparing = on compare
