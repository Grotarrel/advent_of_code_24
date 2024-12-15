
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split ( splitOn )
import Data.List (nub, delete, foldl', sortBy)
import Data.Set (Set)
import qualified Data.Set as S


data Rel = Inf Int Int deriving Show
type UpdateMap = Map Int Int
type Update = [Int]

data PartialOrder = PartialOrder [Int] (Map Int [Int]) deriving Show

type Chain = [Int]

elements :: PartialOrder -> [Int]
elements (PartialOrder _ graph) = M.keys graph

minima :: PartialOrder -> [Int]
minima (PartialOrder ms _) = ms

successors :: PartialOrder -> Int -> [Int]
successors (PartialOrder _ graph) x = succs 
    where 
        Just succs = M.lookup x graph

isSuccessor :: PartialOrder -> Int -> Int -> Bool
isSuccessor graph x y = y `elem` successors graph x

predecessors :: PartialOrder -> Int -> [Int]
predecessors partial y = [x | x <- elements partial, isSuccessor partial x y]


chainsTo :: PartialOrder -> Int -> [Chain]
chainsTo graph x = do
    m <- minima graph
    chainsBetween graph m x

chainsFrom :: PartialOrder -> Int -> [Chain]
chainsFrom graph x = let succs = successors graph x in 
    if null succs then [[x]] else do
        succ <- succs
        chain <- chainsFrom graph succ 
        return (x:chain)


chainsBetween :: PartialOrder -> Int -> Int -> [Chain]
chainsBetween graph x y = 
    if x == y then [[x]] else do
        let succs = successors graph x
        succ <- succs 
        chain <- chainsBetween graph succ y
        return (x:chain)

chains :: PartialOrder -> [Chain]
chains graph = do
    m <- minima graph 
    chainsFrom graph m

isLower :: PartialOrder -> Int -> Int -> Bool
isLower partial x y =  [] /= chainsBetween partial x y



infList :: PartialOrder -> Int -> [Int]
infList partial x = nub $ concat $ chainsTo partial x

supList :: PartialOrder -> Int -> [Int]
supList partial x = nub $ concat $ chainsFrom partial x




removeRel :: PartialOrder -> Rel -> PartialOrder
removeRel partial@(PartialOrder mins graph) (Inf a b) =
    let succs = successors partial a in 
        PartialOrder mins (M.insert a (delete b succs) graph)

removeRels :: PartialOrder -> [Rel] -> PartialOrder
removeRels = foldl' removeRel





addRel :: PartialOrder -> Rel -> PartialOrder
addRel partial@(PartialOrder mins graph) (Inf a b) | b `notElem` elements partial =
    addRel (PartialOrder mins (M.insert b [] graph)) (Inf a b)

addRel partial@(PartialOrder mins graph) (Inf a b) | a `notElem` elements partial =
    PartialOrder (filter (/= b) (a:mins)) (M.insert a [b] graph)

addRel partial@(PartialOrder mins graph) (Inf a b) | not (isLower partial a b) = 
    let newPartial = PartialOrder (filter (/= b) mins) (M.adjust (b: ) a graph) in 
        removeRels newPartial ([Inf x b | x <- predecessors partial b, isLower partial x a] 
            ++ [Inf a y | y <- successors partial a, isLower partial b y])

addRel partial (Inf a b) | otherwise =
    removeRels partial [Inf x y | x <- infList  partial a, y <- supList partial b, isSuccessor partial x y] 






buildPartial :: [Rel] -> PartialOrder
buildPartial = foldl' addRel (PartialOrder [] M.empty)




middle :: [Int] -> Int
middle xs = let m = length xs `div` 2 in
    xs !! m





readRel :: String -> Rel
readRel s = Inf (read a) (read b) 
    where
        (a:b:_) = splitOn "|" s

readUpd :: String -> Update
readUpd = map read . splitOn ","

readInput :: String -> ([Rel], [Update])
readInput input = 
    let (rel_str, _:upd_str) = break (== "") (lines input) in
        (map readRel rel_str, map readUpd upd_str)





processUpdate :: [Rel] -> Update -> Maybe Int 
processUpdate relations update = 
    let updateSet = S.fromList update
        restrRel = filter (\(Inf a b) -> (S.member a updateSet) && (S.member b updateSet)) relations
        partial = buildPartial restrRel
        chain = head $ chains partial
    in
        if chain == update then Nothing else Just (middle chain)


processUpdates :: [Rel] -> [Update] -> Int
processUpdates graph updates = sum $ catMaybes [processUpdate graph update | update <- updates]






main :: IO ()
main = do
    input <- readFile "input5.txt"
    let (relations, updates) = readInput input 
    print $ processUpdates relations updates



