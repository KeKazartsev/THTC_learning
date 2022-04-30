module Lib (
    cutQuotes,
    mapSwapStrict,
) where

import Data.Tuple
import qualified Data.Map.Strict as M

cutQuotes x = reverse $ tail $ reverse $ tail x

mapSwapStrict :: (Ord a) => M.Map k a -> M.Map a k
mapSwapStrict m = M.fromListWith (\a b -> undefined) swl
    where swl = map swap $ M.toList m
