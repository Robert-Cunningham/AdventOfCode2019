module Day2.Personal where

import Data.List.Split
import Data.Maybe (isJust, fromJust)
--import Streaming.Prelude (iterateM)
import Control.Monad.Extra


main :: IO ()

main = interact $ show . partb . input

input = map (read :: String -> Int) . splitOn ","

parta = run

partb ops = filter (\x -> (program x) !! 0 == 19690720) (map run (getAllOps ops)) !! 0

--data State = State Int [Int] deriving Show

data State = State {idx :: Int, program :: [Int]} deriving Show

getAllOps:: [Int] -> [[Int]]
--getAllOps ops loc = map (\x -> replace ops loc x) [0..99]
getAllOps ops = do
    x <- [0..99]
    y <- [0..99]
    return $ replace (replace ops 1 y) 2 x

--getAllPossibilities:: [Int] -> [[Int]]
--getAllPossibilities ops = do
--    x <- getAllOps ops 1

runStates :: Maybe State -> Maybe State
runStates x = last $ takeWhile isJust (iterate advance x)

run :: [Int] -> State
run x = fromJust $ runStates (Just (State 0 x))
--iterateMaybe = takeWhile isJust iterate
--run = iterateMaybe advance

--advance :: (State a) => a -> Maybe a
advance :: Maybe State -> Maybe State
advance (Just s@(State index ops))
--    | ops !! index == 1 = Just (State (index + 4) ops)
    | ops !! index == 1 = Just (State (index + 4) (let
        s = executeFunctionOnLocs ops (index + 1) (index + 2) (+)
        in replace ops (ops !! (index + 3)) s))
    | ops !! index == 2 = Just (State (index + 4) (let
        s = executeFunctionOnLocs ops (index + 1) (index + 2) (*)
        in replace ops (ops !! (index + 3)) s))
    | ops !! index == 99 = Nothing

advance Nothing = Nothing

executeFunctionOnLocs :: [Int] -> Int -> Int -> (Int -> Int -> Int) -> Int
executeFunctionOnLocs ops i1 i2 fn = (ops !! (ops !! i1)) `fn` (ops !! (ops !! i2))

--replace :: (Integral i) => [a] -> i -> a -> [a]
replace :: [a] -> Int -> a -> [a]
replace i l n = let (before, after) = splitAt l i in before ++ [n] ++ (drop 1 after)