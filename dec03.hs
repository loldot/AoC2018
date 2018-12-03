import Text.Regex.Base
import Text.Regex.Posix
import Data.Map (Map)
import qualified Data.Map as Map

data Rect = Rect Int Int Int Int deriving (Show)

main =  do
    text <- readFile "C:\\temp\\input\\dec03.txt"
    let ls = lines text
    let rects = map readSquare ls
    print $ solvePartOne rects

solvePartOne:: [Rect] -> Int
solvePartOne rects = length(filter (\x -> snd x) (Map.toList (foldr mapSquare Map.empty rects)))

mapSquare::Rect -> Map (Int,Int) Bool -> Map (Int,Int) Bool
mapSquare (Rect pX pY w h) m = foldr add m [(i,j) | i <- [pX..pX + w - 1], j <- [pY..pY+h - 1]]
    where add pos m' = if Map.member pos m'
            then Map.update (\_ -> Just True) pos  m'
            else Map.insert pos False m'

readSquare:: String -> Rect
readSquare str = Rect x y w l
    where (x:y:w:l:_) = drop 1 $ map (\x -> read x ::Int) [ head ls | ls <- str =~ "[0-9]+" ::[[String]]] 

