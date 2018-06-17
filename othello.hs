module Main where
import Utils
import Othello.Data
import Othello.Logic
import Data.Char
import Data.List
import System.IO (hFlush, stdout, getLine)
import Control.Monad (zipWithM)

display Black = " ◼︎ "
display White = " ☐ "
display Empty = " · "

printRow row rowNum = putStr $ (show rowNum) ++ " " ++ row ++ "\n"

printBoard board =
  let numRows = size board
      rows = slice numRows (tiles board)
      rowStrings = map (concatMap display) rows
  in do
    putStrLn $ "   " ++ ([1..numRows]
                         |> map (64 +)
                         |> map (\x -> [chr x])
                         |> intercalate "  ")
    zipWithM printRow rowStrings [1..]

parseMove :: String -> (Int, Int)
parseMove (x : y : _) = (((ord $ toLower x) - (ord 'a')), (digitToInt y) - 1)

showPlayer :: Player -> String
showPlayer player =
  let playerColor = display $ color player
      number = case player of
                 Player1 -> "1"
                 Player2 -> "2"
  in "Player " ++ number ++ " (" ++ playerColor ++ ")"


run gameState = do
  putStr $ replicate 100 '\n' -- clear screen
  printBoard (board gameState)
  putStrLn ""

  case gameState of
    (GameOver board) ->
      case (winner board) of
        Nothing -> putStrLn "It's a tie!"
        Just player -> putStrLn $ "Game over: " ++ (showPlayer player)  ++ " wins!"
    (Continue board player) ->
      do
        let playerColor = color player

        putStrLn $ (showPlayer player)
        putStr "Make a move : "
        hFlush stdout
        input <- getLine

        let (x,y) = parseMove $ input
        let nextMove = (Move playerColor x y)
        let legalMove = isLegalMove board nextMove

        if not legalMove
          then run gameState
          else do
            let nextBoard = makeMove board nextMove
            let nextState = case findNextPlayer nextBoard player of
                              Just nextPlayer -> Continue nextBoard nextPlayer
                              Nothing -> GameOver nextBoard
            run nextState

main = run (Continue initialBoard Player1)
