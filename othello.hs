import Data.Char
import Data.List
import System.IO (hFlush, stdout, getLine)

--
-- Data types
--

data Tile = Empty
          | Black
          | White
          deriving (Eq, Show)

data Board = Board { size :: Int, tiles :: [Tile] }
           deriving (Eq, Show)

data Player = Player1
            | Player2
            deriving (Eq, Show)

color Player1 = Black
color Player2 = White

data GameState = GameOver { board :: Board }
               | Continue { board :: Board, nextPlayer :: Player }
               deriving (Eq, Show)

data Move = Move { tile :: Tile, x :: Int, y :: Int }

--
-- Helper functions
--

replaceNth :: Int -> a -> [a] -> [a]
replaceNth 0 newVal (_ : rest) = newVal : rest
replaceNth n newVal (first : rest) = first : (replaceNth (n-1) newVal rest)

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n list = [take n list] ++ slice n (drop n list)

trim = dropWhileEnd isSpace . dropWhile isSpace

-- "pipe" operator
a |> f = f a


--
-- Game logic
--

setTile :: Tile -> Int -> Int -> Board -> Board
setTile newTile x y (Board n tiles) =
  let i = x + y * n
  in (Board n (replaceNth i newTile tiles))

flip :: Tile -> Tile
flip Empty = Empty
flip Black = White
flip White = Black

put :: Tile -> Tile -> Tile
put Empty other = other
put Black other = Black
put White other = White

directions = [(-1, -1), (0, -1), (1, -1),
              (-1, 0), (1, 0),
              (-1, 1), (0, 1), (1, 1)]

neighbourCoords :: Int -> (Int, Int) -> [(Int, Int)]
neighbourCoords boardSize (x, y) =
  directions
  |> map (\(i, j) -> (x + i, y + j))
  |> filter (\(i, j) -> i >= 0 && i < boardSize && j >= 0 && j < boardSize)


-- TODO
legalMove :: Board -> Move -> Bool
legalMove _ _ = True

-- TODO find which tiles can be flipped from the given move
findFlippable :: Board -> Int -> Int -> Tile -> Board
findFlippable board _ _ _ = board

-- TODO Return a new board with the complete result of a given move
move :: Board -> Tile -> (Int,Int) -> Board
move board tile (x,y) =
  let flippable = findFlippable board x y tile
  in setTile tile x y board

-- Deside whose turn it is
-- TODO Check a given board and who made the last turn, then inspect the board to decide who makes the next move
turn :: Board -> Player -> Player
turn _ Player1 = Player2
turn _ Player2 = Player1


--
-- Constants
--

emptyBoard = (Board 8 (replicate 64 Empty))

initialBoard = emptyBoard
               |> setTile White 3 3
               |> setTile Black 4 3
               |> setTile Black 3 4
               |> setTile White 4 4


--
-- Run Game
--

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
