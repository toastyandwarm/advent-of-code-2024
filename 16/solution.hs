import Data.List
import Data.Graph
import Data.Maybe
import Data.Array
import Debug.Trace

data Square = Wall | Square {visited :: Bool, dir :: Maybe Direction, distance :: Maybe Int, start :: Bool, end :: Bool}

data Direction = DirUp | DirDown | DirLeft | DirRight deriving Eq

instance Show Square where
    show Wall = "#"
    show Square {dir=Just x} = show x
    show Square {dir=Nothing} = "."

instance Show Direction where
    show x
      | x == DirUp = "^"
      | x == DirDown = "v"
      | x == DirLeft = "<"
      | x == DirRight = ">"

toSquare :: Char -> Maybe Square
toSquare '#' = Just Wall
toSquare '.' = Just Square {visited=False, dir=Nothing,       distance=Nothing, start=False, end=False}
toSquare 'S' = Just Square {visited=True,  dir=Just DirRight, distance=Just 0,  start=True,  end=False}
toSquare 'E' = Just Square {visited=False, dir=Nothing,       distance=Nothing, start=False, end=True}
toSquare _ = Nothing

toDirection :: Char -> Maybe Direction
toDirection '^' = Just DirUp
toDirection 'v' = Just DirDown
toDirection '<' = Just DirLeft
toDirection '>' = Just DirRight
toDirection _ = Nothing

addDir :: Direction -> (Int, Int) -> (Int, Int)
addDir dir (x, y) = case dir of
        DirUp -> (x-1, y)
        DirDown -> (x+1, y)
        DirLeft -> (x, y-1)
        DirRight -> (x, y+1)

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

makeGraph :: [[Char]] -> (Graph, (Vertex -> ((Int, Int), (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
makeGraph grid = graphFromEdgeList grid . concat . map (\(x, _) -> x) . concat . flipSecond . map (map (\(x, row) -> foldl' (combine x) ([], (-1, '#')) row)) . map (indexed . map indexed) . (\x -> [x, transpose x]) $ grid

graphFromEdgeList :: [[Char]] -> [((Int, Int), (Int, Int))] -> (Graph, (Vertex -> ((Int, Int), (Int, Int), [(Int, Int)])), ((Int, Int) -> Maybe Vertex))
graphFromEdgeList grid edgePairs = 
    graphFromEdges [((x, y), (x, y), [b | (a, b) <- edgePairs, a == (x, y)]) | (x, row) <- indexed grid, (y, value) <- indexed row]

makeArray :: [[Char]] -> (Array (Int, Int) Square)
makeArray grid = listArray ((0, 0), ((length grid)-1, (length (grid!!0))-1)) . concat . map (map (fromJust . toSquare)) $ grid

combine :: Int -> ([((Int, Int), (Int, Int))], (Int, Char)) -> (Int, Char) -> ([((Int, Int), (Int, Int))], (Int, Char))
combine x (edges, (prevy, prevv)) (nexty, nextv)
  | condition = ((((x, prevy), (x, nexty)):((x, nexty), (x, prevy)):edges), (nexty, nextv))
  | otherwise = (edges, (nexty, nextv))
  where condition = (prevv == '.' || prevv == 'E' || prevv == 'S') && (nextv == '.' || nextv == 'E' || nextv == 'S')

flipSecond :: [[([((Int, Int), (Int, Int))], (Int, Char))]] -> [[([((Int, Int), (Int, Int))], (Int, Char))]]
flipSecond [a, xs] = [a,  map (\(x, y) -> ((map (\((a, b), (c, d)) -> ((b, a), (d, c))) x), y)) xs]

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

printGrid :: (Array (Int, Int) Square) -> String
printGrid arr = concat $ [concat [show (arr!(x, y)) | y <- [0..140]] ++ "\n" | x <- [0..23]] ++ ["\n\n"] ++ [concat [show (arr!(x, y)) | y <- [0..140]] ++ "\n" | x <- [117..140]]
--printGrid arr = concat $ [concat [show (arr!(x, y)) | y <- [0..14]] ++ "\n" | x <- [0..14]]

type ArrayGrid = Array (Int, Int) Square
type GFunc = (Vertex -> ((Int, Int), (Int, Int), [(Int, Int)]))
type GFunc2 = ((Int, Int) -> Maybe Vertex)

-- getDir from to
getDir :: (Int, Int) -> (Int, Int) -> Maybe Direction
getDir (a, b) (c, d)  = case (a-c, b-d) of
    (1, 0) -> Just DirUp
    (-1, 0) -> Just DirDown
    (0, 1) -> Just DirLeft
    (0, -1) -> Just DirRight
    _ -> Nothing

getUpdate :: ArrayGrid -> (Int, Int) -> (Int, Int) -> [((Int, Int), Square)]
getUpdate arr coord neighbour = case distance toSquare of
        Nothing -> [newSquare]
        Just x -> case compare x newDist of
                      EQ -> undefined
                      GT -> [newSquare]
                      LT -> []
    where fromSquare = arr!coord
          dirMove = fromJust $ getDir coord neighbour
          toSquare = arr!neighbour
          newDist = (if (fromJust $ dir $ fromSquare) == (dirMove) then (+1) else (+1001)) $ fromJust $ distance fromSquare
          newSquare = (neighbour, Square {visited=True, dir=Just dirMove, distance=Just newDist, start=start toSquare, end=end toSquare})

updateArray :: ArrayGrid -> Graph -> GFunc -> GFunc2 -> [(Int, Int)] -> (ArrayGrid, [(Int, Int)])
updateArray arr graph gfunc gfunc2 = (\x -> (doUpdate arr x, map (\(a, _) -> a) x)) . concat . concat . map (\(coord, neighbours) -> map (getUpdate arr coord) neighbours) . map (\coord -> let (_, _, neighbours) = gfunc $ fromJust $ gfunc2 coord in (coord, neighbours))

doUpdate :: ArrayGrid -> [((Int, Int), Square)] -> ArrayGrid
doUpdate arr [] = arr
doUpdate arr ((update@(pos, square)):updates) = case distance (arr!pos) of
    Nothing -> doUpdate (arr // [update]) updates
    Just x -> case compare x (fromJust $ distance square) of
        EQ -> doUpdate arr updates
        GT -> doUpdate (arr // [update]) updates
        LT -> doUpdate arr updates

findStart :: ArrayGrid -> (Int, Int)
findStart = (\(a, _) -> a) . head . filter (\(_, b) -> case b of
                                                           Wall -> False 
                                                           Square {start=s} -> s) . assocs
findEnd :: ArrayGrid -> (Int, Int)
findEnd = (\(a, _) -> a) . head . filter (\(_, b) -> case b of
                                                         Wall -> False 
                                                         Square {end=e} -> e) . assocs

iterArray :: ArrayGrid -> Graph -> GFunc -> GFunc2 -> [(Int, Int)] -> ArrayGrid
iterArray arr graph gfunc gfunc2 coords = case (nub newCoords) of
        [] -> newArray
        _ -> iterArray newArray graph gfunc gfunc2 newCoords
    where (newArray, newCoords) = updateArray (trace (printGrid arr) arr) graph gfunc gfunc2 coords

solve1 :: ArrayGrid -> Int
solve1 arr = fromJust $ distance $ traceOut $ arr!(findEnd arr) 

solve2 :: ArrayGrid -> ArrayGrid -> Int -> Int
solve2 arr1 arr2 target = length $ (\x -> trace (test arr1 x) x) $ filter (\x -> case (arr1!x, arr2!x) of
        (Square {distance=a}, Square {distance=b}) ->  elem (traceOut ((fromJust a) + (fromJust b))) [target, target+1000, target-1000]
        _ -> False
    ) $ indices arr1

test :: (Array (Int, Int) Square) -> [(Int, Int)] -> String
test arr matches = concat $ [concat [if (elem (x, y) matches) then "O" else case (arr!(x, y)) of
        Wall -> "#"
        _ -> " "
    | y <- [0..140]] ++ "\n" | x <- [0..140]]

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let (graph, gfunc, gfunc2) = makeGraph parsed
--       print graph
       let array = makeArray parsed
--       putStrLn $ printGrid array
       let solvedStart = iterArray array graph gfunc gfunc2 [findStart array]
       let solvedEnd = iterArray (array // [(findStart array, Square {visited=False, dir=Nothing, distance=Nothing, start=True, end=False}),
                                            (findEnd array, Square {visited=True, dir=(dir $ (solvedStart!) $ findEnd solvedStart), distance=Just 0, start=False, end=True})]) graph gfunc gfunc2 [findEnd array]
       let solved1 = solve1 solvedStart
       let solved2 = solve2 solvedStart solvedEnd solved1
       print solved1
       print solved2
