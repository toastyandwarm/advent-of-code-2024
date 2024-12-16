import qualified Data.List.Split as S
import Data.Maybe
import Data.List
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

toTuple :: [a] -> Maybe (a, a)
toTuple [] = Nothing
toTuple (_:[]) = Nothing
toTuple (x:y:_) = Just (x, y)

data Square = Robot | Box | BoxL | BoxR | Block | Empty deriving Eq

instance Show Square where
    show x
      | x == Robot = "@"
      | x == Box = "O"
      | x == BoxL = "["
      | x == BoxR = "]"
      | x == Block = "#"
      | x == Empty = "."

toSquare :: Char -> Maybe Square
toSquare '@' = Just Robot
toSquare 'O' = Just Box
toSquare '[' = Just BoxL
toSquare ']' = Just BoxR
toSquare '#' = Just Block
toSquare '.' = Just Empty
toSquare _ = Nothing

data Direction = DirUp | DirDown | DirLeft | DirRight deriving Eq

instance Show Direction where
    show x
      | x == DirUp = "^"
      | x == DirDown = "v"
      | x == DirLeft = "<"
      | x == DirRight = ">"

toDirection :: Char -> Maybe Direction
toDirection '^' = Just DirUp
toDirection 'v' = Just DirDown
toDirection '<' = Just DirLeft
toDirection '>' = Just DirRight
toDirection _ = Nothing

apply :: ((a -> b), (c -> d)) -> (a, c) -> (b, d)
apply (f, g) (x, y) = (f x, g y)

splitInput :: [String] -> (Array (Int, Int) Square, [Direction])
splitInput inputs = apply ((\x -> listArray ((0, 0), ((length x)-1, (length $ head x)-1)) (concat $ map (map (fromJust . toSquare)) x)) . split "\n",
                           map (fromJust . toDirection) . concat . split "\n"
                          ) $ fromJust $ toTuple inputs

--   y ->
-- x
-- |
-- v

getPushRow :: (Array (Int, Int) Square) -> Direction -> [(Int, Int)]
getPushRow arr dir = case dir of
        DirUp -> [(x, roboty) | x <- reverse [0..robotx]]
        DirDown -> [(x, roboty) | x <- [robotx..]]
        DirLeft -> [(robotx, y) | y <- reverse [0..roboty]]
        DirRight -> [(robotx, y) | y <- [roboty..]]
    where (robotx, roboty) = (\(a, _) -> a) $ head $ filter (\(_, b) -> b==Robot) $ assocs arr

firstWhere :: Show a => (a -> Bool) -> [a] -> Maybe a
firstWhere _ [] = Nothing
firstWhere f (x:xs) = if f x then Just x else firstWhere f xs

addDir :: Direction -> (Int, Int) -> (Int, Int)
addDir dir (x, y) = case dir of
        DirUp -> (x-1, y)
        DirDown -> (x+1, y)
        DirLeft -> (x, y-1)
        DirRight -> (x, y+1)

doMove :: (Array (Int, Int) Square) -> Direction -> (Array (Int, Int) Square)
doMove arr dir = case arr!square of
        Empty -> arr // [(square, Box), ((addDir dir rpos), Robot), (rpos, Empty)]
        Block -> arr
    where square = fromJust $ firstWhere (\x -> arr!x==Empty || arr!x==Block) $ getPushRow arr dir
          rpos = (\(a, _) -> a) $ head $ filter (\(_, b) -> b==Robot) $ assocs arr

solve1 :: ((Array (Int, Int) Square), [Direction]) -> Int
solve1 = sum . map (\(x, y) -> 100*x + y) . map (\(a, _) -> a) . filter (\(_, b) -> b==Box) . assocs . uncurry (foldl doMove)

printGrid :: (Array (Int, Int) Square) -> String
printGrid arr = concat $ [concat [show (arr!(x, y)) | y <- [0..99]] ++ "\n" | x <- [0..49]]

consequentPushes :: (Array (Int, Int) Square) -> Direction -> [(Int, Int)] -> Maybe [(Int, Int)]
consequentPushes arr dir = (\x -> if elem Block (map (arr!) x) then Nothing else Just x) . nub . concat . map (\(a, b) -> case arr!(a, b) of
        BoxL -> [(a, b), (a, b+1)]
        BoxR -> [(a, b), (a, b-1)]
        _ -> [(a, b)]
    )  . map (addDir dir)

needToPush :: (Array (Int, Int) Square) -> Direction -> [(Int, Int)] -> Maybe [(Int, Int)]
needToPush arr dir pushes = case nextPushes of
        Nothing -> Nothing
        Just xs -> if (filteredPushes == [] || filteredPushes == pushes) then Just pushes else case tail of
            Nothing -> Nothing
            Just tail' -> Just (pushes ++ filteredPushes ++ tail')
    where filteredPushes = filter (\x -> arr!x /= Empty) $ fromJust nextPushes
          nextPushes = consequentPushes arr dir pushes
          tail = needToPush arr dir filteredPushes

doMove2 :: (Array (Int, Int) Square) -> Direction -> (Array (Int, Int) Square)
doMove2 arr dir = case needToPush arr dir [rpos] of
        Nothing -> arr
        Just pospos -> arr // [(pos, Empty) | pos <- pospos] // [((addDir dir pos), arr!pos) | pos <- pospos] // [(rpos, Empty)]
    where rpos = (\(a, _) -> a) $ head $ filter (\(_, b) -> b==Robot) $ assocs arr

solve2 :: ((Array (Int, Int) Square), [Direction]) -> Int
solve2 = sum . map (\(x, y) -> 100*x + y) . map (\(a, _) -> a) . filter (\(_, b) -> b==BoxL) . assocs . uncurry (foldl (\x y -> doMove2 x y))

main :: IO()
main = do
       fileinp1 <- readFile "input.txt"
       fileinp2 <- readFile "input2.txt"
       let parsed1 = splitInput $ S.splitOn "\n\n" fileinp1
       let parsed2 = splitInput $ S.splitOn "\n\n" fileinp2
       let solved1 = solve1 parsed1
       let solved2 = solve2 parsed2
       print solved1
       print solved2
