
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split

data Rel = Inf Int Int
type UpdateMap = Map Int Int
type Update = [Int]

toMap :: Update -> UpdateMap
toMap xs = M.fromList (zip xs [1..])

processRel :: String -> Rel
processRel s = Inf (read a) (read b) 
    where
        a:b:_ = splitOn "|" s

processUpd :: String -> Update
processUpd = map read . splitOn ","

processInput :: String -> ([Rel], [Update])
processInput input = 
    let (rel_str, _:upd_str) = break (== "") (lines input) in
        (map processRel rel_str, map processUpd upd_str)
    

consistent :: UpdateMap -> Rel -> Bool
consistent update (Inf a b) = fromMaybe True (do
    i <- update M.!? a 
    j <- update M.!? b 
    return (i < j))

checkUpdate :: Update -> [Rel] -> Bool
checkUpdate update relations = 
    let updMap = toMap update in
        and [consistent updMap rel | rel <- relations]

middle :: Update -> Int
middle update = let m = length update `div` 2 in
    update !! m



main :: IO ()
main = do
    input <- readFile "input5.txt"
    let (relations, updates) = processInput input 
    print $ sum [middle update | update <- updates, checkUpdate update relations]



