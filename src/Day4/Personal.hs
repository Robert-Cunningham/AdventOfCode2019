module Day4.Personal where

import Control.Monad (guard)
import Data.List (sort, isInfixOf)

possibilities :: [Int]
possibilities = [246515..739105]
--possibilities = [223450]

solutions = length $ do
    numba <- possibilities
    guard . (==6) . length . show $ numba
    guard $ (show numba) == (sort $ show numba)
    guard $ any (\x -> ((((show x) ++ (show x)) `isInfixOf` (show numba)) && not (((show x) ++ (show x) ++ (show x)) `isInfixOf` (show numba)))) [0..9]
    return numba

main = print solutions