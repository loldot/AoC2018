f = map (\s -> read s ::Int) $ words "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

main =  do
    text <- readFile "C:\\temp\\input\\dec08.txt"
    let treeData = map (\s -> read s ::Int) $  words text
    print(solvePartOne treeData)


solvePartOne nums = sum (readMeta [] [] nums)

readMeta::[Int]->[Int]->[Int]->[Int]
readMeta [] [] [] = []
readMeta [] [] (c:n:xs) = readMeta [c] [n] xs
readMeta (0:nodeStack) (m:metaStack) xs = take m xs ++ readMeta (nodeStack) (metaStack) (drop m xs)
readMeta (s:nodeStack) (m:metaStack) (c:n:xs) = readMeta (c:(s - 1):nodeStack) (n:m:metaStack) xs