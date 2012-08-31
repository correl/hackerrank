module Main where

import Data.List (sort, findIndices)
import System.Random (randomRIO)

testboard :: String
testboard = "O_X_X_O__"

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

findEmpty :: String -> [Int]
findEmpty board = findIndices (\c -> c == '_') board

sets :: [[Int]]
sets = [[0,1,2],
        [3,4,5],
        [6,7,8],
        
        [0,3,6],
        [1,4,7],
        [2,5,8],
        
        [0,4,8],
        [2,4,6]]

findMoves :: String -> Char -> [(Int, Int)]
findMoves board player = moves
  where moves = [(scoreIndex i, i) | i <- indexes]
        indexes = findEmpty board
        scoreIndex x = sum $ map (scoreSet player) $ findSets board x

bestMoves :: [(Int, Int)] -> [Int]
bestMoves [] = []
bestMoves moves = [snd m | m <- sortedMoves, bestScore == fst m]
  where sortedMoves = reverse.sort $ moves
        bestScore = fst $ head sortedMoves

findSets :: String -> Int -> [String]
findSets board index = sets''
  where sets' = filter (\x -> elem index x) sets 
        sets'' = map chars' sets'
        chars' = map (\x -> board !! x)

scoreSet :: Char -> String -> Int
scoreSet player set
  | (elem 'O' set) && (elem 'X' set) = 0
  | otherwise = 2 ^ (length.filter (\x -> x /= '_') $ set)

otherPlayer :: Char -> Char
otherPlayer 'X' = 'O'
otherPlayer 'O' = 'X'

coords :: Int -> (Int, Int)
coords index = (y, x)
  where y = index `div` 3
        x = index `rem` 3

main = do

    -- If player is X, I'm the first player.
    -- If player is O, I'm the second player.
    player <- getLine

    -- Read the board now. The board is a list of strings filled with X, O or _.
    board <- getList 3

    let moves = bestMoves $ findMoves (concat board) (player !! 0)
    r <- randomRIO (1, length(moves))
    let nextMove = coords $ moves !! (r - 1)
    
    -- Proceed with processing and print 2 integers separated by a single space.
    putStrLn.(\(x, y) -> show x ++ " " ++ show y) $ nextMove
     
