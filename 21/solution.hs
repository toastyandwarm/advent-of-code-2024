import Data.List
import Data.Maybe
import Data.Array
import Debug.Trace
import Data.Function.Memoize

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

--    y ->
-- x
-- |
-- v

data Direction = DNothing | DUp | DDown | DLeft | DRight deriving (Enum, Eq, Ord, Ix, Bounded)
instance Show Direction where
    show DNothing = "A"
    show DUp = "^"
    show DDown = "v"
    show DLeft = "<"
    show DRight = ">"
instance Memoizable Direction where memoize = memoizeFinite

pathAvoid :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [[Direction]]
pathAvoid (a, b) (c, d) (x, y) = if (x == c) && (abs(d-y) <= abs(d-b)) then [(take (abs (c-a)) $ repeat dx) ++ (take (abs (d-b)) $ repeat dy)]
                            else if (y == d) && (abs(c-x) <= abs(c-a)) then [(take (abs (d-b)) $ repeat dy) ++ (take (abs (c-a)) $ repeat dx)]
                            else nub [(take (abs (c-a)) $ repeat dx) ++ (take (abs (d-b)) $ repeat dy), (take (abs (d-b)) $ repeat dy) ++ (take (abs (c-a)) $ repeat dx)]
    where xcomp = compare a c
          ycomp = compare b d
          dx = case xcomp of
              EQ -> DNothing
              LT -> DUp
              GT -> DDown
          dy = case ycomp of
              EQ -> DNothing
              LT -> DLeft
              GT -> DRight

numpad = array ('0', 'A') [('0', (3, 1)),
                           ('1', (2, 0)),
                           ('2', (2, 1)),
                           ('3', (2, 2)),
                           ('4', (1, 0)),
                           ('5', (1, 1)),
                           ('6', (1, 2)),
                           ('7', (0, 0)),
                           ('8', (0, 1)),
                           ('9', (0, 2)),
                           ('A', (3, 2))]

dirpad = array (DNothing, DRight) [(DNothing, (0, 2)),
                                   (DUp,      (0, 1)),
                                   (DDown,    (1, 1)),
                                   (DLeft,    (1, 0)),
                                   (DRight,   (1, 2))]

memopath = memoize3 pathAvoid

--partial!
path :: Ix a => (Int, Int) -> Array a (Int, Int) -> a -> [a] -> [[Direction]]
path _ _ _ [] = []
path avoid pad a (x:[]) = map (++[DNothing]) $ memopath (pad!x) (pad!a) avoid
path avoid pad a (x:y:xs) = [p ++ [DNothing] ++ q | p <- memopath (pad!x) (pad!a) avoid, q <- path avoid pad x (y:xs)]

memopath2 = memoize (path (0, 0) dirpad DNothing)

path' :: Int -> [Direction] -> Int
path' 0 = length
path' n = sum . map (min1 . map (memopath' (n-1)) . memopath2) . map (\x -> x ++ [DNothing]) . memosplit

memosplit = memoize (split [DNothing])

splitInput :: String -> [String]
splitInput = split "\n"

--partial!
min1 :: Ord a => [a] -> a
min1 xs = foldl1 (\a b -> if a < b then a else b) xs

solve1 :: [(Int, [[Direction]])] -> Int
solve1 [] = 0
solve1 ((x, dirs):xs) = (min1 $ map length $ foldl (.) id (take 2 $ repeat (concat . map (path (0, 0) dirpad DNothing))) dirs)*x  + solve1 xs

solve2 :: [(Int, [[Direction]])] -> Int
solve2 = sum . map (\(x, dirs) -> x*(min1 $ map (memopath' 25) dirs))

memopath' = memoize2 path'

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let dirs :: [(Int, [[Direction]])] = map (\x -> (read $ init x, path (3, 0) numpad 'A' x)) parsed
       let solved1 = solve1 dirs
       let solved2 = solve2 dirs
       print solved1
       print solved2
