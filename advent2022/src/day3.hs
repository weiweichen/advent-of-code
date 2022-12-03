import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
  part1
  part2

common xs ys = [ x | x <- xs , y <- ys, x == y]
common3 xs ys zs = [ x | x <- xs , y <- ys, z <- zs, x == y && x == z && y == z]

part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day3.txt")
    let r0 = map(\line -> splitAt ((Text.length line)  `div` 2) (Text.unpack line)) input_list
    let r1 = map(\pair -> common (fst pair) (snd pair)) r0
    let r2 = map(\c -> if fromEnum (c !! 0) < 91 then fromEnum(c!!0) - 38 else fromEnum(c!!0) - 96) r1
    print (sum r2)

mygroup :: Int -> [a] -> [[a]]
mygroup _ [] = []
mygroup n l
  | n > 0 = (take n l) : (mygroup n (drop n l))
  | otherwise = error "Negative or zero n"

part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day3.txt")
    let r0 = mygroup 3 input_list
    let r1 = map(\list -> map(\t -> Text.unpack t) list) r0
    let c = common3 ((r1 !! 0)!!0) ((r1 !! 0)!!1) ((r1 !! 0)!!2)
    let r2 = map(\list -> common3 (list !!0) (list !! 1) (list !! 2)) r1
    let r3 = map(\c -> if fromEnum (c !! 0) < 91 then fromEnum(c!!0) - 38 else fromEnum(c!!0) - 96) r2
    print (sum r3)

