import Data.Char

main =  do
    text <- readFile "C:\\temp\\input\\dec05.txt"
    print(solvePartTwo(text))

solvePartOne polymer = length(reduce "" polymer)

solvePartTwo polymer = let reductions = map (\c -> reduceIgnore "" (reduce "" polymer) c) "abcdefghijklmnopqrstuvwxyz"
    in minimum [length r | r <- reductions]
    
reduceIgnore::[Char]->[Char]->Char->String
reduceIgnore r [] c = r
reduceIgnore r "\n" c = r
reduceIgnore [] polymer c = reduceIgnore [head polymer] (tail polymer) c
reduceIgnore r (p:polymer) c
    | p == c || invert(c) == p = reduceIgnore r polymer c
    | last r == invert(p) = reduceIgnore (init r) polymer c
    | otherwise = reduceIgnore (r ++ [p]) polymer c

reduce::[Char]->[Char]->String
reduce r [] = r
reduce r "\n" = r
reduce [] polymer = reduce [head polymer] (tail polymer)
reduce r (p:polymer)
    | last r == invert(p) = reduce (init r) polymer
    | otherwise = reduce (r ++ [p]) polymer

invert c
    | isUpper(c) = toLower(c)
    | otherwise = toUpper(c)