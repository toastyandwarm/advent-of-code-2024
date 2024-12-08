import Data.List

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: [Char] -> String -> [String]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x

splitInput :: String -> [((Int, Int), Char)]
splitInput text = concat $ map (\(x, zs) -> map (\(y, z) -> ((x, y), z)) zs) $ map (\(x, zs) -> (x, filter (\(_, z) -> z /= '.') zs)) $ indexed $ map indexed $ split ['\n'] text

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..((length xs)-1)] xs

addCoords :: Num a => (a, a) -> (a, a) -> (a, a)
addCoords (a, b) (c, d) = (a+c, b+d)

checkBounds :: (Num a, Ord a) => (a, a) -> (a, a) -> Bool
checkBounds (c, d) (a, b) = (0 <= a) && (0 <= b) && (a < c) && (b < d)

solve1 :: [((Int, Int), Char)] -> Int
solve1 xs = length $ nub $ filter (checkBounds (50, 50)) $ concat $ concat $ map (\((x0, y0), a) -> map (\((x1, y1), b) -> if (a==b && ((x0, y0) /= (x1, y1))) then [(x0+x0-x1, y0+y0-y1), (x1+x1-x0, y1+y1-y0)]  else []) xs) xs

solve2 :: [((Int, Int), Char)] -> Int
solve2 xs = length $ nub $ filter (checkBounds (50, 50)) $ concat $ concat $ concat $ map (\((x0, y0), a) -> map (\((x1, y1), b) -> if a==b then (map (\n -> [(x0+n*(x0-x1), y0+n*(y0-y1)), (x1+n*(x1-x0), y1+n*(y1-y0))]) [1..50]) else []) xs) xs

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
