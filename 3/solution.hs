import Data.List
import Text.Regex

solve1 :: String -> Int -> Int
solve1 string curr = 
    case matchRegexAll (mkRegex "mul\\(([1-9][0-9]?[0-9]?),([1-9][0-9]?[0-9]?)\\)") string of
        Nothing -> curr
        Just (_, _, rem, matched) -> solve1 (rem) (curr + (foldl (*) 1 (map read matched)))

solve2 :: String -> Int -> Bool -> Int
solve2 string curr status = 
    case matchRegexAll (mkRegex "mul\\(([1-9][0-9]?[0-9]?),([1-9][0-9]?[0-9]?)\\)|(do\\(\\))|(don't\\(\\))") string of
        Nothing -> curr
        Just (_, _, rem, [_, _, "do()", _]) -> solve2 (rem) curr True
        Just (_, _, rem, [_, _, _, "don't()"]) -> solve2 (rem) curr False
        Just (_, _, rem, matched) -> solve2 (rem) (curr + (if status then (foldl (*) 1 (map read (take 2 matched))) else 0)) status

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let solved1 = solve1 fileinp 0
       let solved2 = solve2 fileinp 0 True
       print solved1
       print solved2
