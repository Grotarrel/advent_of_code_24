import Data.List

readInputLine :: String -> [Int]
readInputLine line = map read (words line)

readInput :: String -> [[Int]]
readInput input = map readInputLine (lines input)

count :: (a -> Bool) -> [a] -> Int
count test = foldl' addTest 0
    where addTest n x = if test x then n+1 else n

satisfiesChainErrors :: (a -> a -> Bool) -> Int -> [a] -> Bool
satisfiesChainErrors _ _ [] = True
satisfiesChainErrors _ _ [x] = True
satisfiesChainErrors cond 0 [x,y] = cond x y
satisfiesChainErrors _ _ [x,y] = True
satisfiesChainErrors cond n (x:y:z:xs) = case (n, cond x y, cond y z) of
    (0, False, _) -> False
    (0, _, False) -> False
    (n, True, True) -> satisfiesChainErrors cond n (y:z:xs)
    (n, False, _) -> satisfiesChainErrors cond (n-1) (x:z:xs) || satisfiesChainErrors cond (n-1) (y:z:xs)
    (n, True, False) -> satisfiesChainErrors cond (n-1) (x:y:xs) || satisfiesChainErrors cond (n-1) (x:z:xs)


isSafe :: [Int] -> Bool
isSafe xs = (satisfiesChainErrors (\x y -> x < y && y - x <= 3) 1 xs) || (satisfiesChainErrors (\x y -> x > y && x - y <= 3) 1 xs)
    
main :: IO ()
main = do
    input <- readFile "input2.txt"
    let reports = readInput input
    print $ count isSafe reports 

