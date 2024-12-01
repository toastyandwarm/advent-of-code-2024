import Data.List

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: [Char] -> [Char] -> [[Char]]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x


toTuple :: [a] -> (a, a)
toTuple (x:(y:ys)) = (x, y)

fromTuple :: (a, a) -> [a]
fromTuple (x, y) = [x, y]

splitInput :: String -> [(Integer, Integer)]
splitInput text = map (\x -> to_tuple $ map read x) $ map (filter (/="")) $ map (split [' ']) $ split ['\n'] text

solve1 :: [(Integer, Integer)] -> Integer
solve1 x = sum $ map (\(x, y) -> abs (x - y)) $ uncurry zip $ toTuple $ map sort $ fromTuple $ unzip x

solve2 :: [(Integer, Integer)] -> Integer
solve2 x = sum $ (\(x, y) -> map (\z -> z * (count z y)) x) $ unzip x

count :: Eq a => a -> [a] -> Integer
count x y = toInteger $ length $ filter (==x) y

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
