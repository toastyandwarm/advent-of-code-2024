import Data.List
import Data.Maybe
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

toTuple :: [a] -> Maybe (a, a)
toTuple [] = Nothing
toTuple (_:[]) = Nothing
toTuple (x:y:_) = Just (x, y)

splitInput :: String -> [((Int, Int), (Int, Int))]
splitInput = map (fromJust . toTuple . map (fromJust . toTuple . map read . tail . split "=,")) . map (split " ") . split "\n"

solve1 :: [((Int, Int), (Int, Int))] -> Int
solve1 = quadrants (101, 103) . map (\(x, y) -> (x `mod` 101, y `mod` 103)) . map (\((x, y), (vx, vy)) -> (x + 100*vx, y+100*vy))

quadrants :: (Int, Int) -> [(Int, Int)] -> Int
quadrants (x, y) pos = (length $ filter (\(a, b) -> (a < x `div` 2) && (b < y `div` 2)) pos) *
                       (length $ filter (\(a, b) -> (a < x `div` 2) && (b > y `div` 2)) pos) *
                       (length $ filter (\(a, b) -> (a > x `div` 2) && (b > y `div` 2)) pos) *
                       (length $ filter (\(a, b) -> (a > x `div` 2) && (b < y `div` 2)) pos)

solve2 :: Int -> [((Int, Int), (Int, Int))] -> String
solve2 n = concat . map ((\x -> x ++ "\n") . concat . map (\x -> if x>0 then show x else " ")) . (\pos -> map (\row -> map (\col -> length $ filter (==(col, row)) pos) [0..100]) [0..102]) . map (\(x, y) -> (x `mod` 101, y `mod` 103)) . map (\((x, y), (vx, vy)) -> (x + n*vx, y+n*vy))

iter = 7572

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 iter parsed
       print solved1
       putStrLn solved2
