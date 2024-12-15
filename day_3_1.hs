import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec as P

findAll :: P.Parsec T.Text () a ->  P.Parsec T.Text () [a]
findAll pattern = many loop 
    where
        loop = try pattern <|> try (anyChar >> loop)

integer :: P.Parsec T.Text () Int
integer = do
    ds <- many1 digit
    return $ read ds

item :: P.Parsec T.Text () (Int, Int)
item = do
    string "mul("
    a <- integer
    char ','
    b <- integer
    char ')'
    return (a,b)

processInput :: T.Text -> [(Int,Int)]
processInput text = xs
    where Right xs = parse (findAll item) "input" text

multiply :: [(Int,Int)] -> Int 
multiply xs = sum $ map (uncurry (*)) xs
    
main :: IO ()
main = do
    input <- TIO.readFile "input3.txt"
    print $ multiply (processInput input)

