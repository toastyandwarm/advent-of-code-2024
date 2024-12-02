import Data.List
import Debug.Trace

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: [Char] -> [Char] -> [[Char]]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x

splitInput :: String -> [[Int]]
splitInput text = map (map read) $ map (filter (/="")) $ map (split [' ']) $ split ['\n'] text

solve1 :: [[Int]] -> Int
solve1 x = length $ filter (\x -> ((decreasing x) || (increasing x)) && (differences x)) x

solve2 :: [[Int]] -> Int
solve2 x = length $ filter (foldl (||) False) $ map (map (\x -> ((decreasing x) || (increasing x)) && (differences x))) $ map modifiedSets x

modifiedSets :: [Int] -> [[Int]]
modifiedSets x = map (removeElem x) ([0..length x])

removeElem :: [Int] -> Int -> [Int]
removeElem [] _ = []
removeElem (x:xs) 0 = xs
removeElem (x:xs) n = x : removeElem xs (n-1)

increasing :: [Int] -> Bool
increasing x = (\(x, y) -> x) $ foldl' (\(x, y) z -> (x && (y < z), z)) (True, 0) x

decreasing :: [Int] -> Bool
decreasing x = (\(x, y) -> x) $ foldr (\z (x, y) -> (x && (y < z), z)) (True, 0) x

differences :: [Int] -> Bool
differences x = (\(x, y) -> x) $ foldl (\(x, y) z -> (x && (0 < abs(y-z) && abs(y-z) < 4), z)) (True, head x-2) x

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
