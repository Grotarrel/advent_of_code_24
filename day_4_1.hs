import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

type Matrix a = V.Vector (V.Vector a)

(!) :: Matrix a -> (Int, Int) -> Maybe a
mat ! (i,j) = do
    l <- mat V.!? i
    l V.!? j

processInput :: String -> Matrix Char
processInput input = V.fromList $ map V.fromList (lines input)

checkDirection :: Int -> Int -> Int -> Int -> Matrix Char -> Bool
checkDirection x y i j mat = 
    fromMaybe False (do
        m <- mat ! (i+x, j+y)
        a <- mat ! (i+2*x, j+2*y)
        s <- mat ! (i+3*x, j+3*y)
        return $ m == 'M' && a == 'A' && s == 'S')


checkAt :: Int -> Int -> Matrix Char -> Int
checkAt i j mat = 
    if mat ! (i,j) == Just 'X' then
        length [(x,y) | x <- [-1,0,1], y <- [-1,0,1], checkDirection x y i j mat]
    else
        0    
    

main :: IO ()
main = do
    input <- readFile "input4.txt"
    let mat = processInput input
    let nb_lin = V.length mat
    let nb_col = V.length (mat V.! 0)
    print $ sum [checkAt i j mat | i <- [0..nb_lin-1], j <- [0..nb_col-1]]

