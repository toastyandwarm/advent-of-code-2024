import Data.List
import Data.Graph
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

splitInput :: String -> [[Char]]
splitInput = split ['\n']

makeGraph :: [[Char]] -> (Graph, (Vertex -> (Char, (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
makeGraph grid = graphFromEdgeList grid . concat . map (\(x, _) -> x) . concat . flipSecond . map (map (\(x, row) -> foldl' (combine x) ([], (-1, '#')) row)) . map (indexed . map indexed) . (\x -> [x, transpose x]) $ grid

graphFromEdgeList :: [[Char]] -> [((Int, Int), (Int, Int))] -> (Graph, (Vertex -> (Char, (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
graphFromEdgeList grid edgePairs = 
    graphFromEdges [(value, (x, y), [b | (a, b) <- edgePairs, a == (x, y)]) | (x, row) <- indexed grid, (y, value) <- indexed row] 

combine :: Int -> ([((Int, Int), (Int, Int))], (Int, Char)) -> (Int, Char) -> ([((Int, Int), (Int, Int))], (Int, Char))
combine x (edges, (prevy, prevv)) (nexty, nextv)
  | nextv == prevv = ((((x, prevy), (x, nexty)):((x, nexty), (x, prevy)):edges), (nexty, nextv))
  | otherwise      = (edges, (nexty, nextv))

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

flipSecond :: [[([((Int, Int), (Int, Int))], (Int, Char))]] -> [[([((Int, Int), (Int, Int))], (Int, Char))]]
flipSecond [a, xs] = [a,  map (\(x, y) -> ((map (\((a, b), (c, d)) -> ((b, a), (d, c))) x), y)) xs]

solve1 :: Graph -> (Vertex -> (Char, (Int, Int), [(Int, Int)])) -> ((Int, Int) -> Maybe Vertex) -> (Array Vertex Int) -> Int
solve1 graph gfunc gfunc2 outdegrees = sum $ concat $ map (map (\x -> (4 - outdegrees!x))) $ map (reachable graph) $ vertices graph

solve2 :: [[Char]] -> Graph -> ((Int, Int) -> Maybe Vertex) -> Int
solve2 grid graph gfunc2 = (sum $ map (checkWindow graph gfunc2) $ squareWindows grid) + (sum $ map (checkEdge graph gfunc2) $ getEdges grid) + (sum $ map (area graph gfunc2) [(0, 0), (0,(length grid)-1), ((length grid)-1, 0), ((length grid)-1, (length grid)-1)])

getEdges :: [[Char]] -> [(Char, Char, (Int, Int, Int, Int))]
getEdges grid = let d = ((length grid)-1) in concat $ map (\x -> [
    (grid!!0!!x, grid!!0!!(x+1), (0, x, 0, x+1)),
    (grid!!d!!x, grid!!d!!(x+1), (d, x, d, x+1)),
    (grid!!x!!0, grid!!(x+1)!!0, (x, 0, x+1, 0)),
    (grid!!x!!d, grid!!(x+1)!!d, (x, d, x+1, d))
    ]) [0..(d-1)]

checkEdge :: Graph -> ((Int, Int) -> Maybe Vertex) -> (Char, Char, (Int, Int, Int, Int)) -> Int
checkEdge graph gfunc2 (a, b, (x, y, z, w))
  | a==b = 0
  | a/=b = (area graph gfunc2 (x, y)) + (area graph gfunc2 (z, w))

checkWindow :: Graph -> ((Int, Int) -> Maybe Vertex) -> (Char, Char, Char, Char, (Int, Int)) -> Int
checkWindow graph gfunc2 (a, b, c, d, (x, y))
  -- AA AB
  -- BB AB
  | (a==b) && (c==d) = 0
  | (a==c) && (b==d) = 0
  -- AA AB AA BA
  -- AB AA BA AA
  | (a==b) && (a==d) = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x, y+1))
  | (a==b) && (a==c) = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x+1, y+1))
  | (a==d) && (a==c) = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x+1, y))
  | (b==d) && (b==c) = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x+1, y))
  -- AB AC CC CA
  -- CC BC AB CB
  | (a==b) = (area graph gfunc2 (x, y+1)) + (area graph gfunc2 (x+1, y+1))
  | (c==d) = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x+1, y))
  | (a==c) = (area graph gfunc2 (x+1, y)) + (area graph gfunc2 (x+1, y+1))
  | (b==d) = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x, y+1))
  -- AB
  -- CD
  | otherwise  = (area graph gfunc2 (x, y)) + (area graph gfunc2 (x, y+1)) + (area graph gfunc2 (x+1, y)) + (area graph gfunc2 (x+1, y+1))

area :: Graph -> ((Int, Int) -> Maybe Vertex) -> (Int, Int) -> Int
area graph gfunc2 coords = length $ reachable graph $ fromJust $ gfunc2 coords

--a : x,   y
--b : x+1, y
--c : x,   y+1
--d : x+1, y+1
-- ac
-- bd

squareWindows  :: [[Char]] -> [(Char, Char, Char, Char, (Int, Int))]
squareWindows grid = [
    (grid!!x!!y,
     grid!!(x+1)!!y,
     grid!!x!!(y+1),
     grid!!(x+1)!!(y+1),
     (x, y)
    ) | x <- [0..((length grid)-2)],
        y <- [0..((length grid)-2)]]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let (graph, gfunc, gfunc2) = makeGraph parsed
       let outdegrees = outdegree graph
       let solved1 = solve1 graph gfunc gfunc2 outdegrees
       let solved2 = solve2 parsed graph gfunc2
       print solved1
       print solved2
