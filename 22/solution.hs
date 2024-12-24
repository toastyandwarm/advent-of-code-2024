import Data.List
import Data.Bits
import Data.Maybe
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

splitInput :: String -> [Integer]
splitInput = map read . split "\n"

evolve :: Integer -> Integer
evolve x = c 
    where a = ((x*64) `xor` x) `mod` 16777216 
          b = ((a `div` 32) `xor` a) `mod` 16777216
          c = ((b*2048) `xor` b) `mod` 16777216

memoEvolve = memoize evolve

firstNumbers :: Int -> Integer -> [Integer]
firstNumbers 0 _ = []
firstNumbers n x = x : (firstNumbers (n-1) (memoEvolve x))

prices :: Integer -> [Integer]
prices = map (`mod` 10) . firstNumbers 2000

patterns :: Num a => [a] -> [(a, (a, a, a, a))]
patterns (a:b:c:d:e:xs) = (e, (b-a, c-b, d-c, e-d)) : (patterns (b:c:d:e:xs))
patterns _ = []

sellPrice :: Eq a => (a, a, a, a) -> [(b, (a, a, a, a))] -> Maybe b
sellPrice _ [] = Nothing
sellPrice pattern ((a, x):xs)
  | pattern == x = Just a
  | otherwise    = sellPrice pattern xs

solve1 :: [Integer] -> Integer
solve1 = sum . withStrategy (parListChunk 64 rdeepseq) . map (last . firstNumbers 2000)

mapPar :: NFData b => Int -> (a -> b) -> [a] -> [b]
mapPar n f xs = (map f xs) `using` parListChunk n rdeepseq

solve2 :: [Integer] -> Integer
solve2 inits = foldl (\a b -> if a > b then a else b) 0 $ mapPar 64 (\p -> sum $ mapPar 128 (\h -> case (sellPrice p h) of
        Nothing -> 0
        Just x -> x
    ) hists) ps
    where hists = map (patterns . prices) inits

test = \x -> (x <= 9) && (-9 <= x)

ps = [(a, b, c, d) | a <- xs, b <- xs, c <- xs, d <- xs, test (a+b), test (b+c), test (c+d), test (a+d), test (a+b+c), test (b+c+d), test (a+b+c+d)]
    where xs = [-9..9]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
