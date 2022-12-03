import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
  part1
  part2

value1 v | v == "X" = 1
         | v == "Y" = 2
         | v == "Z" = 3

fight1 a b | a ++ b == "AX" = 3
          | a ++ b == "BX" = 0
          | a ++ b == "CX" = 6
          | a ++ b == "AY" = 6
          | a ++ b == "BY" = 3
          | a ++ b == "CY" = 0
          | a ++ b == "AZ" = 0
          | a ++ b == "BZ" = 6
          | a ++ b == "CZ" = 3
          | otherwise = 0

fight2 a b | a ++ b == "AX" = 3
           | a ++ b == "BX" = 1
           | a ++ b == "CX" = 2
           | a ++ b == "AY" = 1
           | a ++ b == "BY" = 2
           | a ++ b == "CY" = 3
           | a ++ b == "AZ" = 2
           | a ++ b == "BZ" = 3
           | a ++ b == "CZ" = 1
           | otherwise = 0

value2 v | v == "X" = 0
         | v == "Y" = 3
         | v == "Z" = 6


part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day2.txt")
    let r0 = map (\line -> Split.splitOn " " (Text.unpack line)) input_list
    let r1 = map(\pair -> ((fight1 (pair !! 0) (pair !! 1)), value1 (pair !! 1))) r0
    let result = sum(map (\t -> fst t + snd t) r1)
    print result

part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day2.txt")
    let r0 = map (\line -> Split.splitOn " " (Text.unpack line)) input_list
    let r1 = map(\pair -> ((fight2 (pair !! 0) (pair !! 1)), value2 (pair !! 1))) r0
    let result = sum(map (\t -> fst t + snd t) r1)
    print result
