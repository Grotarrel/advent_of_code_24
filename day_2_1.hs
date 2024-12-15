import Data.List

readInputLine :: String -> [Int]
readInputLine line = map read (words line)

readInput :: String -> [[Int]]
readInput input = map readInputLine (lines input)

count :: (a -> Bool) -> [a] -> Int
count test = foldl' addTest 0
    where addTest n x = if test x then n+1 else n


isSafeMono :: Ordering -> [Int] -> Bool
isSafeMono _ [] = True
isSafeMono _ [x] = True
isSafeMono mon (x:y:xs) = (mon == compare x y) && abs (x - y) <= 3 && isSafeMono mon (y:xs)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [x] = True
isSafe (x:y:xs) = let comp = compare x y in
    case comp of
        EQ -> False
        _ -> isSafeMono comp (x:y:xs)
    
main :: IO ()
main = do
    input <- readFile "input2.txt"
    let reports = readInput input
    print $ count isSafe reports

