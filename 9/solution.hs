import Data.List
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

splitInput :: String -> [Int]
splitInput = map (\x -> read [x]) . head . split ['\n']

solve1 :: [Int] -> Int
solve1 = sum . zipWith (\a b -> a*b) [0..] . replaces (-1) . concat . zipWith (\z y -> replicate y z) ids

solve2 :: [Int] -> Int
solve2 = sum . zipWith (\a b -> a*(if b == (-1) then 0 else b)) [0..] . concat . map (\(Block {blockID=x, blockLength=y}) -> replicate y x) . replaces2 . map makeBlock . zip ids

-- [Block id length]

makeBlock :: (Int, Int) -> Block
makeBlock (a, b) = Block {blockID=a, blockLength=b}

data Block = Block {blockID :: Int, blockLength :: Int}

instance Eq Block where
    a == b = (blockLength a) == (blockLength b) && (blockID a) == (blockID b)

instance Show Block where
    show (Block{blockID=a, blockLength=b}) = show (a, b)

replaces2 :: [Block] -> [Block]
replaces2 [] = []
replaces2 (x:[]) = [x]
replaces2 (x:xs)
  | (blockLength x) == 0 = replaces2 xs
  | (blockID x) /= (-1) = (x) : (replaces2 xs)
  | (blockID (last xs)) == (-1) = replaces2 (x:(init xs)) ++ [last xs]
  | otherwise = case getFirstGap (x:xs) (last xs) of
        Nothing -> (replaces2 (init (x:xs))) ++ [last xs]
        Just y  -> replaces2 $ replacePair y ((last xs), Block{blockID=(-1), blockLength=((blockLength y) - (blockLength (last xs)))}) (x:(init xs)) ++ [Block{blockID=(-1), blockLength=(blockLength (last xs))}]

getFirstGap :: [Block] -> Block -> Maybe Block
getFirstGap [] _ = Nothing
getFirstGap (x:xs) y
  | (blockID x == (-1)) && ((blockLength x) >= (blockLength y)) = Just x
  | otherwise                                                   = getFirstGap xs y

replacePair :: Eq a => a -> (a, a) -> [a] -> [a]
replacePair _ _ [] = []
replacePair a (b, c) (x:xs)
  | x == a    = b:c:xs
  | otherwise = x:(replacePair a (b, c) xs)

merge :: [a] -> [a] -> [a]
merge (x:xs) ys = x:(merge ys xs)

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs)
  | x == a    = b:xs
  | otherwise = x:(replace a b xs)

replaces :: (Eq a, Show a) => a -> [a] -> [a]
replaces _ [] = []
replaces a xs
  | not (elem a xs) = xs
  | otherwise       = replaces a . init $ replace a (last xs) (traceShow (length xs) xs)

ids :: [Int] = merge [0..] (repeat (-1))

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
