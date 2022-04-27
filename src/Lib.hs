module Lib (
    cutQuotes,
) where

cutQuotes x = reverse $ tail $ reverse $ tail x
