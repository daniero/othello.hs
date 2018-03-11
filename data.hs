module Othello.Data where

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
