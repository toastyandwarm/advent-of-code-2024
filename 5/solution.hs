import Data.List
import qualified Data.List.Split as S
import Debug.Trace

data PageNum = Page Integer [(Integer, Integer)]

instance Num PageNum where
    (Page x r) + (Page y _) = Page (x+y) r
    (Page x r) - (Page y _) = Page (x-y) r

instance Eq PageNum where
    (Page x _) == (Page y _) = x == y

instance Ord PageNum where
    (Page a zs) <= (Page b _) = foldl (&&) True $ map (\x -> checkRule x [a, b])  zs

instance Show PageNum where
    show (Page a _) = "Page " ++ (show a)

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
splitInput text = map (\x -> toTuple $ map read x) $ map (filter (/="")) $ map (split ['|']) $ split ['\n'] text

splitInput2 :: String -> [[Integer]]
splitInput2 text = map (\x -> map read x) $ map (filter (/="")) $ map (split [',']) $ split ['\n'] text

checkRule :: (Integer, Integer) -> [Integer] -> Bool
checkRule _ [] = True
checkRule (a, b) (x:xs) 
  | a == x              = True
  | b == x && elem a xs = False
  | b == x              = True
  | otherwise           = checkRule (a, b) xs

solve1 :: [(Integer, Integer)] -> [[Integer]] -> Integer
solve1 [] y = sum $ map (\x -> x!!(div ((length x)-1) 2)) y
solve1 (x:xs) y = solve1 (xs) (filter (\y -> checkRule x y) y)

solve2 :: [(Integer, Integer)] -> [[Integer]] -> PageNum
solve2 a [] = Page 0 a
solve2 rules (x:xs) = (getMid $ sort $ map (\a -> Page a rules) x) + (solve2 rules xs)

getMid :: [a] -> a
getMid xs = xs !! (div ((length xs) -1) 2)

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let sections = S.splitOn "\n\n" fileinp
       let parsed = splitInput (sections!!0)
       let parsed2 = splitInput2 (sections!!1)
       let solved1 = solve1 parsed parsed2
       let solved2 = (solve2 parsed parsed2) - (Page solved1 [])
       print solved1
       print solved2
