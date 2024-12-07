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


splitInput :: String -> [(Int, [Int])]
splitInput text = map (\(x:xs) -> (x, xs)) $ map (map read) $ map (filter (/="")) $ map (split [' ', ':']) $ split ['\n'] text

solve1 :: [(Int, [Int])] -> Int
solve1 [] = 0
solve1 ((a, (b:bs)):xs)
  | bs == []  = solve1 xs
  | otherwise = ((\x -> if x then 0 else a) $ null $ filter (==a) $ map (\fs -> (foldl' (flip ($)) b (zipWith flip fs bs))) (getOperators (length bs))) + solve1 xs

getOperators :: (Num a, Show a) => Int -> [[(a -> a -> a)]]
getOperators 0 = [[]]
getOperators x
  | x < 0     = [[]]
  | otherwise = (map (\x -> (+):x) (getOperators (x-1))) ++ (map (\x -> (*):x) (getOperators (x-1)))

solve2 :: [(Int, [Int])] -> Int
solve2 [] = 0
solve2 ((a, (b:bs)):xs)
  | bs == []  = solve2 xs
  | otherwise = ((\x -> if x then 0 else a) $ null $ filter (==a) $ map (\fs -> (foldl' (flip ($)) b (zipWith flip fs bs))) (getOperators2 (length bs))) + solve2 xs

getOperators2 :: (Num a, Show a, Read a) => Int -> [[(a -> a -> a)]]
getOperators2 0 = [[]]
getOperators2 x
  | x < 0     = [[]]
  | otherwise = (map (\x -> (+):x) (getOperators2 (x-1))) ++ (map (\x -> (*):x) (getOperators2 (x-1))) ++ (map (\x -> concat':x) (getOperators2 (x-1)))

concat' :: (Show a, Read a) => a -> a -> a
concat' x y = read ((show x) ++ (show y))

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
