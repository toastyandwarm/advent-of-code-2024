import Data.List
import Data.Function.Memoize
import Debug.Trace
import Control.Parallel.Strategies

traceOut :: Show a => a -> a
traceOut x = (traceShow x x)

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: [Char] -> String -> [String]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x

splitInput :: String -> [Int]
splitInput = map read . split [' '] . head . split ['\n']

solve1 :: [Int] -> Int
solve1 = length . foldl (.) id (take 25 $ repeat (concat . map stoneStep))

solve2 :: [Int] -> Integer
solve2 xs = sum $ map (memocount 40000) xs -- ((map ((memoize2 stoneCount) 40000) xs) `using` parList rseq)

stoneStep :: Int -> [Int]
stoneStep 0 = [1]
stoneStep x
  | (floor $ logBase 10 $ fromIntegral x) `rem` 2 == 1 = let d = ((\x -> x `div` 2) $ (+1) $ floor $ logBase 10 $ fromIntegral x) in [x `div` 10^d, x `rem` 10^d]
  | otherwise                                          = [x*2024] 

splitAtIndex :: Int -> [a] -> [[a]]
splitAtIndex 0 xs = [[], xs]
splitAtIndex a (x:xs) = addFirst (splitAtIndex (a-1) xs) x

stoneCount :: Int -> Int -> Integer
stoneCount 0 _ = 1
stoneCount n x 
  | x == 0 = memocount (n-1) 1
  | (floor $ logBase 10 $ fromIntegral x) `rem` 2 == 1 = let d = ((\x -> x `div` 2) $ (+1) $ floor $ logBase 10 $ fromIntegral x) in (memocount (n-1) (x `rem` 10^d)) + (memocount (n-1) (x `div` 10^d))
  | otherwise = memocount (n-1) (x*2024)
-- sum . map (((memoize2 stoneCount)) (n-1)) . stoneStep

memocount = memoize2 stoneCount

main :: IO()
main = do
       --fileinp <- readFile "input.txt"
       --let parsed = splitInput fileinp
       let parsed = [1]
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
