import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
  part1
  part2


part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day1.txt")
    let result = maximum(map (\l -> sum(map (\x -> read (Text.unpack x) :: Integer) l)) (Split.splitOn (Text.pack(""):[]) input_list))
    print result

sortGT a b
      | a < b  = GT
      | a > b = LT
      | otherwise = EQ


part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day1_1.txt")
    let r0 = (map (\l -> sum(map (\x -> read (Text.unpack x) :: Integer) l)) (Split.splitOn (Text.pack(""):[]) input_list))
    let r1 = sortBy sortGT r0
    let result = sum(take 3 r1)
    print result

