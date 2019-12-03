--main :: IO ()

--count :: (Num a, String s) => s
--count s = show (length s)

--Why not:
--count s = show (length . lines s)
--main = interact count

--main = interact (count . lines)


--main :: IO ()
--main = interact $ \s -> show (length . lines $ s)

main :: IO ()

main = interact (show . calculateFuel . lines)

calculateFuel l = sum([read(x) `div` 3 - 2 | x <- l])