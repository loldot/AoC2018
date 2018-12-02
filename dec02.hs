import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

main =  do
    text <- readFile "C:\\temp\\input\\dec02.txt"
    let codes = lines text
    solvePartTwo codes

solvePartOne = print . checksum

solvePartTwo codes = print(findFirstByDist 1 codes)

findFirstByDist n codes = fst(head(filter ((==n).snd) (map compareCode (combinations codes))))

combinations [] = []
combinations (code:codes) = [(code,c) | c <- codes] ++ combinations codes

compareCode (codeA, codeB) = 
    let common = [ca | (ca, cb) <- zip codeA codeB, ca == cb]
        diff =  [cb | (ca, cb) <- zip codeA codeB, ca /= cb]
    in (common, length(diff))

checksum codes =
    let  hists = map (\c -> histogram c Map.empty) codes
    in (countOccurance 2 hists) * (countOccurance 3 hists)

countOccurance n hists = length(filter (hasOccurance n) hists)

hasOccurance n map = any ((==n).snd) (Map.toList map)   

histogram::String-> Map Char Int -> Map Char Int
histogram [] m = m
histogram (x:xs) m = if Map.member x m
        then histogram xs (Map.adjust (+1) x m)
        else histogram xs (Map.insert x 1 m)