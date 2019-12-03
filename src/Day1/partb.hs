main :: IO ()

main = interact $ show . sum . (map $ recursiveFuelCalculate) . (map (read :: String -> Int)) . lines

calculateFuel :: Integral a => a -> a
calculateFuel x = x `div` 3 - 2

calculateFuelList :: Integral a => [a] -> a
calculateFuelList x = sum $ map calculateFuel x

recursiveFuelCalculate :: Integral a => a -> a
recursiveFuelCalculate weight
    | fuel <= 0 = 0
    | otherwise = fuel + recursiveFuelCalculate(fuel)
    where fuel = calculateFuel(weight)

--show recursiveFuelCalculate 100756