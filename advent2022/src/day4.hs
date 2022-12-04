import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
  part1
  part2

contains :: [[Integer]]->Bool
contains l = (((l !! 0) !! 0 <= (l !! 1)!!0) && ((l !! 0) !! 1 >= (l!!1)!!1)) || (((l !! 0) !! 0 >= (l !! 1)!!0) && ((l !! 0) !! 1 <= (l!!1)!!1))


not_overlap:: [[Integer]]->Bool
not_overlap l = ((l !! 0) !! 1 < (l !! 1) !! 0) || ((l !! 0) !! 0 > (l !! 1) !! 1)

part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day4.txt")
    let r0 = map (\line -> Split.splitOn "," (Text.unpack line)) input_list
    let r1 = map (\pair -> map(\t-> Split.splitOn "-" (t)) pair) r0
    let r2 = map(\list -> map(\t -> map(\x -> read x :: Integer) t) list) r1
    let r3 = filter(\pair -> contains pair) r2
    print (length r3)

part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day4.txt")
    let r0 = map (\line -> Split.splitOn "," (Text.unpack line)) input_list
    let r1 = map (\pair -> map(\t-> Split.splitOn "-" (t)) pair) r0
    let r2 = map(\list -> map(\t -> map(\x -> read x :: Integer) t) list) r1
    let r3 = filter(\pair -> not_overlap pair) r2
    print ((length r2) - (length r3))
