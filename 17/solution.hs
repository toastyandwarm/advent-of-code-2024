import Data.List
import Data.Maybe
import Data.Bits
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

--partial!
splitInput :: String -> State
splitInput text = (uncurry3 State $ fromJust $ toTuple $ map (read . last . split ": ") $ take 3 lines) 0 (map (\(a, b) -> (toEnum a :: Opcode, b)) $ pair $ map read $ split "," $ last $ split ": " $ last lines)
    where lines = split ['\n'] text

toTuple :: [a] -> Maybe (a, a, a)
toTuple (x:y:z:_) = Just (x, y, z)
toTuple _ = Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

pair :: [a] -> [(a, a)]
pair [] = []
pair (x:[]) = []
pair (x0:x1:xs) = (x0, x1):(pair xs)

data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Enum, Show, Eq)
data State = State {regA :: Int, regB :: Int, regC :: Int, pointer :: Int, instructions :: [(Opcode, Int)]} deriving (Show)

combo :: State -> Int -> Int
combo state x
  | x <= 3 = x
  | x == 4 = regA state
  | x == 5 = regB state
  | x == 6 = regC state
  | otherwise = undefined

runMachine :: State -> [Int]
runMachine x = if ((point `div` 2) >= (length inst)) then [] else case opcode of
        Adv -> runMachine $ State (valA `div` (2^comb)) valB valC (point + 2) inst
        Bxl -> runMachine $ State valA (valB `xor` arg) valC (point + 2) inst
        Bst -> runMachine $ State valA (comb `mod` 8) valC (point + 2) inst
        Jnz -> runMachine $ if valA == 0 then State valA valB valC (point + 2) inst
                                         else State valA valB valC (arg) inst
        Bxc -> runMachine $ State valA (valB `xor` valC) valC (point + 2) inst
        Out -> (comb `mod` 8) : (runMachine $ State valA valB valC (point + 2) inst)
        Bdv -> runMachine $ State valA (valA `div` (2^comb)) valC (point + 2) inst
        Cdv -> runMachine $ State valA valB (valA `div` (2^comb)) (point + 2) inst
    where valA = regA x
          valB = regB x
          valC = regC x
          point = pointer x
          inst = instructions x
          (opcode, arg) = inst!!(point `div` 2)
          comb = combo x arg

solve1 :: State -> String
solve1 = foldl (++) "" . intersperse "," . map show . runMachine

-- done manually
solve2 :: State -> Int
solve2 state = 90938893795561

main :: IO()
main = do
       fileinp <- readFile "input.txt"
       let parsed = splitInput fileinp
       print parsed
       let solved1 = solve1 parsed
       let solved2 = solve2 parsed
       print solved1
       print solved2
