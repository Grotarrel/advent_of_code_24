import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec as P
import Control.Monad (void)

integer :: P.Parsec T.Text a Int
integer = do
    ds <- many1 digit
    return $ read ds

item :: P.Parsec T.Text a (Int, Int)
item = do
    string "mul("
    x <- integer
    char ','
    y <- integer
    char ')'
    return (x,y)

doParser :: P.Parsec T.Text Bool ()
doParser = do
    string "do()"
    P.putState True 

dontParser :: P.Parsec T.Text Bool ()
dontParser = do
    string "don't()"
    P.putState False

fillerParser :: P.Parsec T.Text Bool ()
fillerParser = try doParser <|> try dontParser <|> void anyChar

myParser :: P.Parsec T.Text Bool [(Int, Int)]
myParser = many loop
    where
        loop = do
            active <- P.getState
            if active then
                try item <|> try (fillerParser >> loop)
            else
                fillerParser >> loop


processInput :: T.Text -> [(Int,Int)]
processInput text = xs
    where Right xs = runParser myParser True "input" text

multiply :: [(Int,Int)] -> Int 
multiply xs = sum $ map (uncurry (*)) xs
    
main :: IO ()
main = do
    input <- TIO.readFile "input3.txt"
    print $ multiply (processInput input)

