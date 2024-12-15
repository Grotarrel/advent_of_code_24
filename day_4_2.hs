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
        s <- mat ! (i-x, j-y)
        return $ m == 'M' && s == 'S')


checkAt :: Int -> Int -> Matrix Char -> Bool
checkAt i j mat = 
    (mat ! (i,j) == Just 'A') &&
        (length [(x,y) | x <- [-1,1], y <- [-1,1], checkDirection x y i j mat ] == 2)
       
    

main :: IO ()
main = do
    input <- readFile "input4.txt"
    let mat = processInput input
    let nb_lin = V.length mat
    let nb_col = V.length (mat V.! 0)
    print $ length [(i,j) | i <- [0..nb_lin-1], j <- [0..nb_col-1], checkAt i j mat]

