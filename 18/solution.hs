import Data.List
import Data.Array
import Data.Graph
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

--partial!
splitInput :: String -> [(Int, Int)]
splitInput = map ((\(a:b:_) -> (a, b)) . map read . split ",") . split "\n"

buildArray :: [(Int, Int)] -> (Array (Int, Int) Square)
buildArray xs = listArray ((0, 0), (70, 70)) (repeat (Square False Nothing False False)) // [((0, 0), (Square False (Just 0) True False)), ((70, 70), (Square False Nothing False True))] // (map (\x -> (x, Wall)) xs)

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((a, b), (c, d)) (x, y) = (a <= x && x <= c && b <= y && y <= d)

neighbours :: (Array (Int, Int) a) -> (Int, Int) -> [(Int, Int)]
neighbours arr (x, y) = filter (inBounds (bounds arr)) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

buildGraph :: (Array (Int, Int) Square) -> (Graph, (Vertex -> ((Int, Int), (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
buildGraph arr = graphFromEdges $ map (\x -> let (coords, value) = x in case value of
        Wall -> (coords, coords, [])
        Square {} -> (coords, coords, (filter (\x -> case (arr!x) of
            Wall -> False
            Square {} -> True) $ neighbours arr coords))) (assocs arr)

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
solve1 arr = fromJust $ distance $ ((dijkstra arr [findStart arr])!(findEnd arr))

solve2 :: [(Int, Int)] -> (Int, Int)
solve2 blocks = (blocks!!) $ (\x -> x-1) $ head $ filter (\x -> let arr = buildArray $ take x $ blocks in (let (graph, gfunc, gfunc2) = buildGraph arr in not $ path graph (fromJust $ gfunc2 $ findStart arr) (fromJust $ gfunc2 $ findEnd arr))) [0..]

printGrid :: (Array (Int, Int) Square) -> String
printGrid arr = concat $ [concat [show (arr!(x, y)) | y <- [0..70]] ++ "\n" | x <- [0..70]]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let arr = buildArray $ take 1024 $ parsed
       let solved1 = solve1 arr
       let solved2 = solve2 parsed
       print solved1
       print solved2
