import Data.List
import Data.Array
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

--    x——>
-- y
-- |
-- v

data Square = Wall | Square {visited :: Bool, distance :: Maybe Int, start :: Bool, end :: Bool}

instance Show Square where
    show x = case x of
        Wall -> "#"
        Square {start=True} -> "S"
        Square {end=True} -> "E"
        Square {visited=True} -> "."
        Square _ _ _ _ -> " "

toSquare :: Char -> Maybe Square
toSquare '#' = Just Wall
toSquare '.' = Just Square {visited=False, distance=Nothing, start=False, end=False}
toSquare 'S' = Just Square {visited=False, distance=Nothing, start=True,  end=False}
toSquare 'E' = Just Square {visited=True,  distance=Just 0,  start=False, end=True}
toSquare _ = Nothing

--partial!
splitInput :: String -> [[Char]]
splitInput = split "\n"

buildArray :: [[Char]] -> (Array (Int, Int) Square)
buildArray grid = listArray ((0, 0), ((length grid)-1, (length (grid!!0))-1)) . concat . map (map (fromJust . toSquare)) $ grid

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((a, b), (c, d)) (x, y) = (a <= x && x <= c && b <= y && y <= d)

neighbours :: (Array (Int, Int) a) -> (Int, Int) -> [(Int, Int)]
neighbours arr (x, y) = filter (inBounds (bounds arr)) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

doUpdate :: (Array (Int, Int) Square) -> [((Int, Int), Square)] -> (Array (Int, Int) Square)
doUpdate arr [] = arr
doUpdate arr ((update@(pos, square)):updates) = case distance (arr!pos) of
    Nothing -> doUpdate (arr // [update]) updates
    Just x -> case compare x (fromJust $ distance square) of
        EQ -> doUpdate arr updates
        GT -> doUpdate (arr // [update]) updates
        LT -> doUpdate arr updates

dijkstra :: (Array (Int, Int) Square) -> [(Int, Int)] -> (Array (Int, Int) Square)
dijkstra arr buffer = if buffer == [] then arr else dijkstra (doUpdate arr updates) (nub $ map (\(x,_) -> x) updates)
    where updates = concat $ map (\x -> concat $ map (\coords -> case (arr!coords) of
            Wall -> []
            Square {visited=True} -> []
            y@(Square {}) -> [(coords, y {visited=True, distance=Just ((fromJust $ distance (arr!x)) + 1)})]) (neighbours arr x)) buffer

findStart :: (Array (Int, Int) Square) -> (Int, Int)
findStart = (\(a, _) -> a) . head . filter (\(_, b) -> case b of
    Wall -> False
    Square {start=s} -> s) . assocs
findEnd :: (Array (Int, Int) Square) -> (Int, Int)
findEnd = (\(a, _) -> a) . head . filter (\(_, b) -> case b of
    Wall -> False
    Square {end=e} -> e) . assocs

--partial!
solve1 :: (Array (Int, Int) Square) -> Int
solve1 arr = length $ filter (>=100) $ concat $ map (\x -> map (\y -> (fromJust $ distance (dists!x)) - (fromJust $ distance (dists!y)) - 2) $ filter (\x -> isSquare (arr!x)) $ concat $ map (neighbours arr) $ neighbours arr x) $ map (\(i, e) -> i)$ filter (\(i, e) -> isSquare e) $ assocs arr
    where dists = (dijkstra arr [findEnd arr])

--partial!
solve2 :: (Array (Int, Int) Square) -> Int
solve2 arr = length $ filter (>=100) $ concat $ map (\x -> map (\y -> (fromJust $ distance (dists!x)) - (fromJust $ distance (dists!y)) - (taxicabDistance x y)) $ filter (\x -> isSquare (arr!x)) $ neighbourhood arr 20 x) $ map (\(i, e) -> i)$ filter (\(i, e) -> isSquare e) $ assocs arr
    where dists = (dijkstra arr [findEnd arr])
printGrid :: (Array (Int, Int) Square) -> String
printGrid arr = concat $ [concat [show (arr!(x, y)) | y <- [0..140]] ++ "\n" | x <- [0..140]]

isSquare :: Square -> Bool
isSquare Wall = False
isSquare Square {} = True

taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (a, b) (c, d) = (abs(a-c)) + (abs(b-d))

neighbourhood :: (Array (Int, Int) Square) -> Int -> (Int, Int) -> [(Int, Int)]
neighbourhood arr r (x, y) = filter (inBounds (bounds arr)) $ map (\(a, b) -> (a+x, b+y)) $ filter (\(a, b) -> (abs a) + (abs b) <= r) [(a, b) | a <- [-r..r], b <- [-r..r]]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let arr = buildArray parsed
       let solved1 = solve1 arr
       let solved2 = solve2 arr
       print solved1
       print solved2
