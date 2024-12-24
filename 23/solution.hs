import Data.List
import Data.Maybe
import Data.Array
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

type Chars = (Char, Char)

--partial!
splitInput :: String -> [(Chars, Chars)]
splitInput = concat . map (\(a:b:[]) -> [(toTuple $ a, toTuple $ b), (toTuple $ b, toTuple $ a)]) . map (split "-") . split "\n"

--partial!
toTuple :: [a] -> (a, a)
toTuple (a:b:_) = (a, b)

getEdges :: Eq a => [(a, a)] -> a -> [a]
getEdges [] _ = []
getEdges ((a, b):edges) x = if x == a then b:remainder 
                       else if x == b then a:remainder 
                       else                  remainder
    where remainder = getEdges edges x

buildArray :: [(Chars, Chars)] -> Array Chars [Chars]
buildArray edges = array (('a', 'a'), ('z', 'z')) [((x, y), getEdges edges (x, y)) | x <- chars, y <- chars]
    where chars = ['a'..'z']

beginsT :: Chars -> Bool
beginsT ('t', _) = True
beginsT _ = False

solve1 :: Array Chars [Chars] -> Int
solve1 arr = (`div` 6) $ length $ nub $ filter (\(a, x, y) -> beginsT a || beginsT x || beginsT y) $ filter (\(a, x, y) -> (inArray arr y) && (elem x (arr!y))) $ concat $ map (\(a, xs) -> [(a, x, y) | x <- xs, y <- xs]) $ assocs arr

maximalCliques :: Ix a => Array a [a] -> [[a]]
maximalCliques arr = f [] (indices arr) []
    where f = \r p x -> case (p, x) of
              ([], []) -> [r]
              _ -> g r p x
          g = \r p x -> case p of
              [] -> []
              (v:vs) -> (f (nub (v:r)) (filter (\x -> elem x (arr!v)) p) (filter (\x -> elem x (arr!v)) x)) ++ (g r vs (nub (v:x)))

inArray :: Ix a => Array a b -> a -> Bool
inArray arr x = elem x (indices arr)

maxBy :: Ord b => (a -> b) -> a -> [a] -> a
maxBy f i xs = foldl (\a b -> if (f a) > (f b) then a else b) i xs

solve2 :: Array Chars [Chars] -> String
solve2 arr = concat $ intersperse "," $ map (\(a, b) -> [a, b]) $ sort $ maxBy length [] $ maximalCliques arr

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let arr = buildArray parsed
       let solved1 = solve1 arr
       let solved2 = solve2 arr
       print solved1
       putStrLn solved2
