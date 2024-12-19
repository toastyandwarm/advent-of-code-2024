import Debug.Trace
import Text.Regex
import Data.List
import qualified Data.List.Split as S

import Data.Function.Memoize
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

splitInput :: String -> [String]
splitInput = filter (/="") . split ", "

splitInput2 :: String -> [String]
splitInput2 = split "\n"

solve1 :: [String] -> [String] -> Int
solve1 patterns towels = length $ filter (match (mkRegex ("^(" ++ (concat $ intersperse "|" patterns) ++ ")*$"))) towels

match :: Regex -> String -> Bool
match regex str = case matchRegex regex str of
    Just _ -> True
    Nothing -> False

solveTowel :: [String] -> String -> Int
solveTowel patterns towel = sum $ ((map (
        \pattern -> case matchRegexAll (mkRegex ('^':pattern)) towel of
            Nothing -> 0
            Just (_, _, remaining, _) -> if remaining == "" then 1 else (memoize2 solveTowel) patterns remaining
    ) patterns) `using` parList rdeepseq)

solve2 :: [String] -> [String] -> Int
solve2 patterns towels = sum $ ((map ((memoize2 solveTowel) patterns) towels) `using` parList rdeepseq)

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let sections = S.splitOn "\n\n" fileinp
       let parsed = splitInput (sections!!0)
       let parsed2 = splitInput2 (sections!!1)
       let solved1 = solve1 parsed parsed2
       let solved2 = solve2 parsed parsed2
       print solved1
       print solved2
