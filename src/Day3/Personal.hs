{-# LANGUAGE ScopedTypeVariables#-}

module Day3.Personal where

import Data.Ord (comparing)
import Data.List.Split
import Linear.V2
import Data.List (sortBy, elemIndex, sort)
import Data.Set (intersection, toList)
import qualified Data.Set as Set
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ show . partb . input

parta :: [[Instruction]] -> [V2 Int]
parta inputs = sorter $ calcIntersect $ map getTouchedPoints inputs

partb inputs = let 
    touched = map getTouchedPoints inputs
    intersectionPoints = calcIntersect touched
    total = sum $ map length touched
    time = map (\p -> total - ((fromJust $ elemIndex p (touched !! 0)) + (fromJust $ elemIndex p (touched !! 1)))) intersectionPoints
    in sort time


sorter = sortBy $ comparing (\(V2 ax ay :: V2 Int) -> (abs ax + abs ay))

data Instruction = Instruction (V2 Int) Int deriving Show

--data Direction = DUp | DDown | DLeft | DRight deriving Show

input raw = map parseLine (lines raw)

parseLine :: String -> [Instruction]
parseLine = map (\i -> (Instruction (letterToDirection . head $ i) ((read :: String -> Int ) . tail $ i))) . splitOn ","

letterToDirection :: Char -> V2 Int
letterToDirection x
    | x == 'U' = V2 1 0
    | x == 'D' = V2 (-1) 0
    | x == 'L' = V2 0 (-1)
    | x == 'R' = V2 0 1

getTouchedPoints :: [Instruction] -> [V2 Int]
getTouchedPoints s = foldl (\history instr -> (reverse $ nInDirection (head $ history) instr) ++ history) [V2 0 0] s

nInDirection :: V2 Int -> Instruction -> [V2 Int]
nInDirection start (Instruction dir amt) = tail $ take (amt + 1) (iterate (+ dir) start)

calcIntersect a = Set.toList $ intersection (Set.fromList (a !! 0)) (Set.fromList (a !! 1))