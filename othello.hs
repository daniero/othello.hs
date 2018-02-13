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

data GameState = GameOver { board :: Board }
               | Continue { board :: Board, nextPlayer :: Player }
               deriving (Eq, Show)


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


move :: Board -> Board
move board = board


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
    (GameOver board) -> putStrLn "Game over: PlayerX wins" -- TODO finn vinnern
    (Continue board player) ->
      do
        putStrLn "PlayerX's turn"
        putStr "Make a move: "
        hFlush stdout
        input <- getLine
        let move = parseMove $ input
        print $ fst move
        print $ snd move
        putStrLn $ ">" ++ (show move) ++ "<"
  
main = run (Continue initialBoard Player1)
