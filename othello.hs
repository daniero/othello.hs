module Main where
import Utils
import Othello.Data
import Othello.Logic
import Data.Char
import Data.List
import System.IO (hFlush, stdout, getLine)

display Black = " ◼︎ "
display White = " ☐ "
display Empty = " ⋅ "

printRow row = putStr $ row ++ "\n"
printBoard board = mapM printRow $ map (concatMap display) (slice 8 (tiles board))

parseMove :: String -> (Int, Int)
parseMove (x : y : _) = (((ord $ toLower x) - (ord 'a')), (digitToInt y) - 1)

run gameState = do
  putStr $ replicate 100 '\n' -- clear screen
  printBoard (board gameState)
  putStrLn ""
  case gameState of
    (GameOver board) -> putStrLn "Game over: PlayerX wins" -- TODO find the winner
    (Continue board player) ->
      do
        let playerColor = color player
        putStrLn $ (show player) ++ " (" ++ (display playerColor) ++ ")"
        putStr $ "Make a move : "
        hFlush stdout
        input <- getLine
        let nextMove = parseMove $ input
        -- TODO evaluate if move is legal (unoccupied squre etc)
        let nextBoard = move board playerColor nextMove
        -- TODO decide next game state based on nextBoard
        let nextPlayer = turn nextBoard player
        let nextState = Continue nextBoard nextPlayer
        run nextState
  
main = run (Continue initialBoard Player1)
