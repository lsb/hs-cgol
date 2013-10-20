import Data.List
import Data.List.Split

type Board = [[Bool]]

xyDims :: [[a]] -> (Int,Int)
xyDims lst = (length lst, length (head lst))


neighbors :: Board -> (Int, Int) -> Int
neighbors board (x,y) = length $ filter id [(board !! xdiff) !! ydiff | xd <- [-1,0,1], yd <- [-1,0,1], let xdiff = (x+xd+xMax) `mod` xMax, let ydiff = (y+yd+yMax) `mod` yMax, not (xd == 0 && yd == 0)]
  where (xMax, yMax) = xyDims board

nextIter :: Board -> (Int, Int) -> Bool
nextIter board (x,y) = if (board !! x) !! y then n == 2 || n == 3 else n == 3
  where n = neighbors board (x,y)

nextBoard :: Board -> Board
nextBoard board = [[nextIter board (x,y) | y <- [0..yMax-1]] | x <- [0..xMax-1]] where (xMax, yMax) = xyDims board


readItercountXYBoard :: [String] -> (Int, Board)
readItercountXYBoard (i:xy:asciiboard) = if sane then (read i, board) else error "DOES NOT COMPUT"
  where [xd,yd] = splitOn " " xy
        xMax = read xd
        yMax = read yd
        board = map (map (== "1") . splitOn " ") asciiboard
        (xMaxComputed, yMaxComputed) = xyDims board
        lengths = map length board
        sane = [yMaxComputed] == nub lengths && yMax == yMaxComputed && xMax == xMaxComputed


main = interact $ unlines . (\ stdin -> let (iterCount, board) = readItercountXYBoard stdin in map (concat . intersperse " " . map (\ b -> if b then "1" else "0")) $ (iterate nextBoard board) !! iterCount) . lines
