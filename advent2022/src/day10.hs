import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Typeable
import Data.Set (toList, fromList)
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names
                                  -- prefixed with "Map." (with the period).
import Data.Graph (Graph)
import qualified Data.Graph as Graph

parse item
  | (item !! 0) == "noop" = (1, 0)
  | otherwise = (2, read (item !! 1) :: Int)

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day10.txt")
    let r0 = map(\line -> Split.splitOn " " (Text.unpack line)) input_list
    let input = map(\item -> parse item) r0
    part1 input
    part2 input

apply i n prev_sum list = do
    if i == n then list else do
        let (cycle, value) = prev_sum
        let (cc, cv) = (list !! i)
        let new_item = (cycle + cc, value + cv)
        let new_list = (take i list) ++ [new_item] ++ (drop (i + 1) list)
        apply (i + 1) n new_item new_list


my_find :: Int -> [(Int, Int)] -> Int
my_find v list = do
    let r0 = foldl(\acc item-> if (fst item) < v then item else acc) (0, 0) list
    snd r0


draw_item :: Int -> (Int, Int)->String->(Int, Int)->String
draw_item t item result next_item = do
    let (upper, _) = next_item
    if t == upper then result
    else do
        let p = snd item
        let t_x = (t `mod` 40)
        let dx = abs (t_x - p)
        let curr_pixel = if dx <= 1 then '#' else '.'
        let new_result = (take t result) ++ [curr_pixel] ++ (drop (t + 1) result)
        draw_item (t + 1) item new_result next_item

draw i n result t list = do
   if i == n then result else do
       let item = (list !! (i - 1))
       let next_item = (list !! i)
       let new_result = draw_item t item result next_item
       draw (i + 1) n new_result (fst next_item) list

part1 input = do
    -- print input
    let r0 = [(0, 1)] ++ (apply 0 (length input) (0, 1) input)
    let r1 = map(\v -> (my_find v r0) * v) [20, 60, 100, 140, 180, 220]
    print (sum r1)

part2 input = do
    -- print input
    let r0 = [(0, 1)] ++ (apply 0 (length input) (0, 1) input)
    let r1 = take 240 (repeat ' ')
    let r2 = draw 0 (length r0) r1 0 r0
    let r3 = take 40 r2
    let r4 = take 40 (drop 40 r2)
    let r5 = take 40 (drop 80 r2)
    let r6 = take 40 (drop 120 r2)
    let r7 = take 40 (drop 160 r2)
    let r8 = take 40 (drop 200 r2)

    print r3
    print r4
    print r5
    print r6
    print r7
    print r8


