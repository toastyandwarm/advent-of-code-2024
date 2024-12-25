import Data.List
import Data.Maybe
import qualified Data.List.Split as S
import Debug.Trace

traceOut :: Show a => a -> a
traceOut x = (traceShow x x)

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x

data Schematic = Key [Int] | Lock [Int] deriving (Eq, Show)

splitInput :: [String] -> [Schematic]
splitInput = map $ (\x -> case x of 
        ((y:_):_) -> (if y == '#' then Lock else Key) $ map (length . filter (=='#')) x
        _ -> Lock []
    ) . transpose . split "\n"

matchKeys :: Schematic -> Schematic -> Bool
matchKeys a b = case (a, b) of
    (Key _, Key _) -> False
    (Lock _, Lock _) -> False
    (Lock x, Key y) -> all (<=7) $ zipWith (+) x y
    (Key x, Lock y) -> all (<=7) $ zipWith (+) x y

solve1 :: [Schematic] -> Int
solve1 xs = (`div` 2) $ sum $ map sum $ map (\x -> map (\y -> if matchKeys x y then 1 else 0) xs) xs

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput $ S.splitOn "\n\n" fileinp
       print parsed
       let solved1 = solve1 parsed
--       let solved2 = solve2 parsed
       print solved1
--       print solved2
