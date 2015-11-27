-- | 'Poset' and 'Sortable' instances for instances of 'Prelude.Ord'
{-# LANGUAGE CPP #-}
module Data.Poset.Instances where

import qualified Data.Poset.Internal as Poset
import Data.Poset.Internal (Poset, Sortable, partialOrder, totalOrder)

import Data.Ratio
import Data.List
import Data.Word
import Data.Int

#define POSET_ORD_INSTANCE(ctx, v) instance ctx Poset (v) where { \
    compare = (partialOrder .) . compare; \
    (<)     = (<); \
    (<=)    = (<=); \
    (>=)    = (>=); \
    (>)     = (>); \
    (<==>)  = const $ const True; \
    (</=>)  = const $ const False }

#define SORTABLE_ORD_INSTANCE(ctx, v) instance ctx Sortable (v) where { \
    isOrdered = const True; \
    sortBy f  = sortBy $ (totalOrder .) . f; \
    max       = max; \
    min       = min; }

#define ORD_INSTANCE(ctx, v) \
    POSET_ORD_INSTANCE(ctx, v); \
    SORTABLE_ORD_INSTANCE(ctx, v); \
    instance ctx Poset.Ord (v)

ORD_INSTANCE(, Bool)
ORD_INSTANCE(, Char)
ORD_INSTANCE(, Int)
ORD_INSTANCE(, Int8)
ORD_INSTANCE(, Int16)
ORD_INSTANCE(, Int32)
ORD_INSTANCE(, Int64)
ORD_INSTANCE(, Word)
ORD_INSTANCE(, Word8)
ORD_INSTANCE(, Word16)
ORD_INSTANCE(, Word32)
ORD_INSTANCE(, Word64)
ORD_INSTANCE(, Integer)

ORD_INSTANCE(Integral a =>, Ratio a)
