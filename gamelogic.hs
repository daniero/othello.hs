module Othello.Logic where
import Othello.Data
import Utils

setTile :: Tile -> (Int, Int) -> Board -> Board
setTile newTile (x, y) (Board n tiles) =
  let i = x + y * n
  in (Board n (replaceNth i newTile tiles))

fill :: Tile -> Tile -> Tile
fill other Empty = other
fill other Black = Black
fill other White = White

flipTile :: Tile -> Tile
flipTile Empty = Empty
flipTile Black = White
flipTile White = Black

directions = [(-1, -1), (0, -1), (1, -1),
              (-1, 0), (1, 0),
              (-1, 1), (0, 1), (1, 1)]

isInside :: Board -> (Int, Int) -> Bool
isInside board (x, y) = x >= 0 &&
                        x < (size board) &&
                        y >= 0 &&
                        y < (size board)

go :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
go (x, y) (i, j) = let next = (x+i, y+j) in next : go next (i, j)

findRuns :: Board -> (Int, Int) -> [[(Int, Int)]]
findRuns board pos =
  directions
  |> map (\dir -> go pos dir
                  |> takeWhile (isInside board))

findFlippable :: Board -> (Int, Int) -> Tile -> Board
findFlippable board pos tile =
  let flippableCoords = (\run ->
                          let tiles = map (tileAt board) run
                              opposites = tiles
                                          |> takeWhile (isOpposite tile)
                                          |> length
                              same = tiles
                                     |> drop opposites
                                     |> takeWhile (== tile)
                                     |> length
                          in
                            if same > 0
                            then run |> take opposites
                            else [])
      addTile = (\board2 pos ->
                  let tile2 = tileAt board pos
                  in setTile tile2 pos board2)
  in
    findRuns board pos
    |> concatMap flippableCoords
    |> foldl addTile emptyBoard

-- TODO
legalMove :: Board -> Move -> Bool
legalMove _ _ = True


move :: Board -> Tile -> (Int,Int) -> Board
move board tile pos =
  findFlippable board pos tile
  |> tiles
  |> map flipTile
  |> zipWith fill (tiles board)
  |> Board (size board)
  |> setTile tile pos

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
               |> setTile White (3, 3)
               |> setTile Black (4, 3)
               |> setTile Black (3, 4)
               |> setTile White (4, 4)

