import qualified Data.List.Split as S
import Data.List
import Data.Matrix as M
import Data.Ratio
import Debug.Trace

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

splitInput :: [String] -> [[[Ratio Int]]]
splitInput = map (map (map ((% 1) . read . last . split "+=") . split [',']). split ['\n'])

solve1 :: [[[Ratio Int]]] -> Ratio Int
solve1 [] = 0
solve1 (x:xs) = (let a = (inverse $ M.transpose $ fromLists ([x!!0, x!!1])) in
    case a of 
        Left _ -> 0
        Right matrix -> let b = matrix * (M.transpose $ fromLists [x!!2]) in if (all (\x -> denominator x == 1) b) then sum (multiplier*b) else 0 
    ) + solve1 xs

solve2 :: [[[Ratio Int]]] -> Ratio Int
solve2 [] = 0
solve2 (x:xs) = (let a = (inverse $ M.transpose $ fromLists ([x!!0, x!!1])) in
    case a of 
        Left _ -> 0
        Right matrix -> let b = matrix * (M.transpose $ shift + fromLists [x!!2]) in if (all (\x -> denominator x == 1) b) then sum(multiplier*b) else 0 
    ) + solve1 xs

multiplier :: Matrix (Ratio Int) = fromLists [[3 % 1, 0], [0, 1 % 1]]
shift :: Matrix (Ratio Int) = fromLists [[10000000000000 % 1, 10000000000000 % 1]]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput $ S.splitOn "\n\n" fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print (numerator $ solved1)
       print (numerator $ solved2)
