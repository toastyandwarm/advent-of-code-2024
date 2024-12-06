{-# LANGUAGE DeriveGeneric #-}
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Containers.ListUtils
import Control.Parallel.Strategies

import GHC.Generics (Generic)

addFirst :: [[a]] -> a -> [[a]]
addFirst [] x = [[x]]
addFirst (y:ys) x = ([x] ++ y) : ys

split :: [Char] -> String -> [String]
split _ [] = []
split delims (x:xs)
  | elem x delims = [[]] ++ split delims xs
  | otherwise     = addFirst (split delims xs) x

splitInput :: String -> BoardState
splitInput text = (\x -> Board (getObstacles x 0) (fromJust $ getGuard x 0) North [] (length x, length (x!!0))) $ split ['\n'] text

getGuard :: [String] -> Int -> Maybe (Int, Int)
getGuard [] _ = Nothing
getGuard (x:xs) y
  | elem '^' x = Just (y, fromJust $ elemIndex '^' x)
  | otherwise  = getGuard xs (y+1)

getObstacles :: [String] -> Int -> [(Int, Int)]
getObstacles [] _ = []
getObstacles (x:xs) y = (map (\x -> (y, x)) $ elemIndices '#' x) ++ (getObstacles xs (y+1))

data Direction = North | East | South | West deriving (Show, Eq, Generic)

instance NFData Direction

-- coordinates of obstacles,
-- position of guard,
-- direction of guard,
-- history of guard,
-- size of grid.
data BoardState = Board [(Int, Int)] (Int, Int) Direction [((Int, Int), Direction)] (Int, Int) deriving Show

increment :: Direction -> Direction
increment North = East
increment East = South
increment South = West
increment West = North

addCoords :: Num a => (a, a) -> (a, a) -> (a, a)
addCoords (a, b) (c, d) = (a+c, b+d)

checkBounds :: (Num a, Ord a) => (a, a) -> (a, a) -> Bool
checkBounds (a, b) (c, d) = (0 <= a) && (0 <= b) && (a < c) && (b < d)

step :: Direction -> (Int, Int)
step North = (-1, 0)
step East = (0, 1)
step South = (1, 0)
step West = (0, -1)

count :: Eq a => a -> [a] -> Int
count x y = length $ filter (==x) y

solve1 :: BoardState -> Int
solve1 x = length $ getHistNub x

getHist :: BoardState -> Maybe [((Int, Int), Direction)]
getHist (Board obstacles pos dir hist size)
  | not (checkBounds pos size)                      = Just hist
  | elem (pos, dir) hist                            = Nothing
  | not (elem (addCoords pos (step dir)) obstacles) = getHist (Board obstacles (addCoords pos (step dir)) dir ((pos, dir):hist) size)
  | otherwise                                       = getHist (Board obstacles pos (increment dir) hist size)

getHistNub :: BoardState -> [((Int, Int), Direction)]
getHistNub x = nubOrdOn (\(a, _) -> a) $ fromJust $ getHist x

solve2 :: BoardState -> Int
solve2 (Board obstacles pos dir hist size) = count Nothing $ ((map (\x -> getHist x) $ map (\(x, y) -> (Board (x:obstacles) pos dir hist size)) $ drop 1 $ getHistNub (Board obstacles pos dir hist size)) `using` parListChunk 64 rdeepseq)

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
