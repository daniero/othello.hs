import Utils
import Othello.Data
import Othello.Logic
import Data.Char
import Data.List
import System.IO (hFlush, stdout, getLine)

display Black = " X "
display White = " O "
display Empty = " · "

printRow row = putStr $ row ++ "\n"
printBoard board = mapM printRow $ map (concatMap display) (slice 8 (tiles board))

parseMove :: String -> (Int, Int)
parseMove (x : y : _) = (((ord $ toLower x) - (ord 'a')), (digitToInt y) - 1)

run gameState = do
  printBoard (board gameState)
  case gameState of
    (GameOver board) -> putStrLn "Game over: PlayerX wins" -- TODO find the winner
    (Continue board player) ->
      do
        putStrLn $ (show player) ++ "'s turn"
        putStr "Make a move: "
        hFlush stdout
        input <- getLine
        let nextMove = parseMove $ input
        -- TODO evaluate if move is legal (unoccupied squre etc)
        let playerColor = color player
        let nextBoard = move board playerColor nextMove
        let nextPlayer = turn nextBoard player
        run (Continue nextBoard nextPlayer)
  
main = run (Continue initialBoard Player1)
