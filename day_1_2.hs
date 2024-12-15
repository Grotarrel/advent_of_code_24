import Data.List
import qualified Data.Map as M
import GHC.Base (VecElem(Int16ElemRep))
import qualified Data.Maybe


readInputLine :: String -> (Int, Int)
readInputLine line =
    let a:b:xs = words line in
        (read a, read b)

readInput :: String -> ([Int], [Int])
readInput input = 
    unzip (map readInputLine (lines input))

lookupDefault :: Ord a => a -> M.Map a Int -> Int
lookupDefault x table = Data.Maybe.fromMaybe 0 (M.lookup x table)

counts :: Ord a => [a] -> M.Map a Int
counts = foldl' upd M.empty
    where upd acc x = 
            let current = lookupDefault x acc
            in M.insert x (current + 1) acc


similarity :: M.Map Int Int -> Int -> Int
similarity table x = x * lookupDefault x table

    
main :: IO ()
main = do
    input <- readFile "input.txt"
    let (xs,ys) = readInput input
    let cs = counts ys
    print $ sum (map (similarity cs) xs)

