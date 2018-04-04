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
  let flippableCoords = (\run -> -- TODO naming; a "run" of coordinates in one direction leading away from the given point
                          let tiles = map (tileAt board) run
                              opposites = tiles
                                          |> takeWhile (isOpposite tile)
                                          |> length
                              same = tiles
                                     |> drop opposites
                                     |> takeWhile (== tile)
                                     |> length
                          in
                            if opposites > 0 && same > 0
                            then run |> take opposites
                            else [])
      addTile = (\newBoard pos ->
                  let tile2 = tileAt board pos
                  in setTile tile2 pos newBoard)
  in
    findRuns board pos
    |> concatMap flippableCoords
    |> foldl addTile emptyBoard

isLegalMove :: Board -> Move -> Bool
isLegalMove board (Move tile x y) =
  if (tileAt board (x, y)) /= Empty
    then False
    else
      let flippable = findFlippable board (x,y) tile
      in any (Empty /=) (tiles flippable)

move :: Board -> Move -> Board
move board (Move tile x y) =
  let pos = (x, y)
  in
    findFlippable board pos tile
    |> tiles
    |> map flipTile
    |> zipWith fill (tiles board)
    |> Board (size board)
    |> setTile tile pos


findAllCoordinates :: Board -> [(Int, Int)]
findAllCoordinates (Board n _) = [(x,y) | x <- [0..n-1], y <- [0..n-1]]

canMove :: Board -> Player -> Bool
canMove board player =
  let playerColor = color player
      coord2move = uncurry (Move playerColor)
  in
      board
      |> findAllCoordinates
      |> map coord2move
      |> any (isLegalMove board)

findNextPlayer :: Board -> Player -> Maybe Player
findNextPlayer board previousPlayer =
  if canMove board (opposite previousPlayer) then Just (opposite previousPlayer)
  else if canMove board previousPlayer then Just previousPlayer
  else Nothing

--
-- Constants
--

emptyBoard = (Board 8 (replicate 64 Empty))

initialBoard = emptyBoard
               |> setTile White (3, 3)
               |> setTile Black (4, 3)
               |> setTile Black (3, 4)
               |> setTile White (4, 4)

