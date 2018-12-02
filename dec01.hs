import Data.Set (Set)
import qualified Data.Set as Set

main =  do
    text <- readFile "C:\\temp\\input\\dec01.txt"
    let ops = lines text
    let adjustments = map readAdjustment ops
    solvePartTwo adjustments
    
solvePartTwo adjs = print(firstDuplicate (cumulativeFrequency (cycle adjs)) Set.empty)

solvePartOne adjs = print(computeFrequency(adjs))

firstDuplicate [] _ = Nothing
firstDuplicate (x:xs) d = if Set.member x d 
                            then Just x
                            else firstDuplicate xs (Set.insert x d)

cumulativeFrequency::Num a => [a]->[a]
cumulativeFrequency = scanl (+) 0

computeFrequency::Num a => [a]->a
computeFrequency = foldl (+) 0

readAdjustment ('+':num) = read num ::Int
readAdjustment ('-':num) = -(read num ::Int)