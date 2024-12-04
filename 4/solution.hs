import Data.List
import qualified Data.List.Split as S
import Debug.Trace

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: [Char] -> [Char] -> [[Char]]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x

splitInput :: String -> [String]
splitInput = split ['\n']

solve1 :: [String] -> Int
solve1 x = sum $ map (\x -> length x -1) $ map (S.splitOn "XMAS") $ rowsColumnsDiags x

solve2 :: [String] -> Int
solve2 xs = length $ filter (\(x, y, z) -> (xs!!(y-1)!!(z+1) == 'M' && xs!!(y+1)!!(z-1) == 'S') || (xs!!(y-1)!!(z+1) == 'S' && xs!!(y+1)!!(z-1) == 'M')) $ filter (\(x, y, z) -> x == 'A' && ((xs!!(y-1)!!(z-1) == 'M' && xs!!(y+1)!!(z+1) == 'S') || (xs!!(y-1)!!(z-1) == 'S' && xs!!(y+1)!!(z+1) == 'M'))) $ concat $ map (drop 1) $ map (\x -> take (length x -1) x) $ drop 1 $ take (length xs - 1) $ addIndexes2 xs

rowsColumnsDiags :: [String] -> [String]
rowsColumnsDiags x = x ++ (map reverse x) ++ (transpose x) ++ (map reverse (transpose x)) ++ (map (getLeadDiag x) [(-length x)..(length x)]) ++ (map (getLeadDiag (reverse x)) [(-length x)..(length x)]) ++ map reverse (map (getLeadDiag x) [(-length x)..(length x)]) ++ map reverse (map (getLeadDiag (reverse x)) [(-length x)..(length x)])

getLeadDiag :: [[a]] -> Int -> [a]
getLeadDiag [] _ = []
getLeadDiag (x:xs) y
  | y < 0         = getLeadDiag xs (y+1)
  | y >= length x = []
  | otherwise     = [x!!y] ++ getLeadDiag xs (y+1)

addIndexes2 :: [[a]] -> [[(a, Int, Int)]]
addIndexes2 xs = map (\x -> map (\y -> ((xs!!x)!!y, x, y)) [0..(length (xs!!x) -1)]) [0..(length xs-1)]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
