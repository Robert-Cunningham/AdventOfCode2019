main :: IO ()

main = interact $ output . partb . input

output = show
input = map (read :: String -> Int) . lines

fuel :: Integral a => a -> a
fuel = subtract 2 . (div 3) --Why can you do this? Aren't we waiting on the first argument? Is this a special feature of infix notation, or is there some way to do this without it?

parta :: Integral a => [a] -> a
parta = sum . map fuel

partb :: Integral a => [a] -> a
--partb ws = sum . map (sum . takeWhile (>= 0) . iterate fuel) $ ws
partb ws = sum $ map (sum . takeWhile (>= 0) . iterate fuel) ws