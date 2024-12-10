import Data.List
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

splitInput :: String -> [[Int]]
splitInput = map (map (\x -> read [x])) . split ['\n']

makeGraph :: [[Int]] -> (Graph, (Vertex -> (Int, (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
makeGraph grid = graphFromEdgeList grid . concat . map (\(x, _) -> x) . concat . flipSecond . map (map (\(x, row) -> foldl' (combine x) ([], (-1, -2)) row)) . map (indexed . map indexed) . (\x -> [x, transpose x]) $ grid

graphFromEdgeList :: [[Int]] -> [((Int, Int), (Int, Int))] -> (Graph, (Vertex -> (Int, (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
graphFromEdgeList grid edgePairs = 
    graphFromEdges [(value, (x, y), [b | (a, b) <- edgePairs, a == (x, y)]) | (x, row) <- indexed grid, (y, value) <- indexed row] 

combine :: Int -> ([((Int, Int), (Int, Int))], (Int, Int)) -> (Int, Int) -> ([((Int, Int), (Int, Int))], (Int, Int))
combine x (edges, (prevy, prevv)) (nexty, nextv)
  | nextv == prevv + 1 = ((((x, prevy), (x, nexty)):edges), (nexty, nextv))
  | nextv == prevv - 1 = ((((x, nexty), (x, prevy)):edges), (nexty, nextv))
  | otherwise          = (edges, (nexty, nextv))

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

flipSecond :: [[([((Int, Int), (Int, Int))], (Int, Int))]] -> [[([((Int, Int), (Int, Int))], (Int, Int))]]
flipSecond [a, xs] = [a,  map (\(x, y) -> ((map (\((a, b), (c, d)) -> ((b, a), (d, c))) x), y)) xs]

solve1 :: Graph -> (Vertex -> (Int, (Int, Int), [(Int, Int)])) -> Int
solve1 graph gfunc = length . concat . map (filter (\v -> let (a, _, _) = gfunc v in a == 9) . reachable graph) $ verticesWhereEq graph gfunc 0

solve2 :: Graph -> (Vertex -> (Int, (Int, Int), [(Int, Int)])) -> ((Int, Int) -> Maybe Vertex) -> Int
solve2 graph gfunc gfunc2 = sum . concat . map (\v0 -> map (\v9 -> paths graph gfunc gfunc2 v0 v9) (verticesWhereEq graph gfunc 9)) $ verticesWhereEq graph gfunc 0

paths :: Graph -> (Vertex -> (Int, (Int, Int), [(Int, Int)])) -> ((Int, Int) -> Maybe Vertex) -> Vertex -> Vertex -> Int
paths graph gfunc gfunc2 x y
  | x == y               = 1
  | not $ path graph x y = 0
  | otherwise            =  sum . map (\x -> paths graph gfunc gfunc2 x y) . map (fromJust . gfunc2) . (\(a, b, c) -> c) $ gfunc x

verticesWhereEq :: Graph -> (Vertex -> (Int, (Int, Int), [(Int, Int)])) -> Int -> [Vertex]
verticesWhereEq graph gfunc x = filter (\v -> let (a, _, _) = gfunc v in a == x) . vertices $ graph

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let (graph, gfunc, gfunc2) = makeGraph parsed
       let solved1 = solve1 graph gfunc
       let solved2 = solve2 graph gfunc gfunc2
       print solved1
       print solved2
