module Othello.Data where

data Tile = Empty
          | Black
          | White
          deriving (Eq, Show)

isOpposite :: Tile -> Tile -> Bool
isOpposite Black White = True
isOpposite White Black = True
isOpposite _ _ = False

data Board = Board { size :: Int, tiles :: [Tile] }
           deriving (Eq, Show)

tileAt :: Board -> (Int, Int) -> Tile
tileAt board (x, y) = (tiles board) !! (y * (size board) + x)

data Player = Player1
            | Player2
            deriving (Eq, Show)

color :: Player -> Tile
color Player1 = Black
color Player2 = White

data GameState = GameOver { board :: Board }
               | Continue { board :: Board, nextPlayer :: Player }
               deriving (Eq, Show)

data Move = Move { tile :: Tile, x :: Int, y :: Int }
