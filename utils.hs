module Utils where
import Data.List
import Data.Char

replaceNth :: Int -> a -> [a] -> [a]
replaceNth 0 newVal (_ : rest) = newVal : rest
replaceNth n newVal (first : rest) = first : (replaceNth (n-1) newVal rest)

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n list = [take n list] ++ slice n (drop n list)

trim = dropWhileEnd isSpace . dropWhile isSpace

-- "pipe" operator
a |> f = f a

