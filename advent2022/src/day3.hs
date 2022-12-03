import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
  part1

part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day2.txt")
    let r0 = map (\line -> Split.splitOn " " (Text.unpack line)) input_list
    let r1 = map(\pair -> ((fight1 (pair !! 0) (pair !! 1)), value1 (pair !! 1))) r0
    let result = sum(map (\t -> fst t + snd t) r1)
    print result

