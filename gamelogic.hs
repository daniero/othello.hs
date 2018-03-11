module Othello.Logic where
import Othello.Data
import Utils

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

