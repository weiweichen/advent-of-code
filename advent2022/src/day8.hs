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


find_xy :: (Int, Int) -> Map (Int, Int) Int -> Int
find_xy name map = case Map.lookup name map of
                 Nothing  -> -1
                 Just s -> s

scan_row :: (Int, Int, Map (Int, Int) Int) -> (Int, Int) -> (Int, Int, Map (Int, Int) Int)
scan_row input h_x = do
  let (prev_h , row, dict) = input
  let (h, x) = h_x
  if prev_h < h then do
     let v = find_xy (row, x) dict
     if v == -1 then do
         let new_dict = Map.insert (row, x) h dict
         (h, row, new_dict)
     else
         (h, row, dict)
  else do
     (prev_h, row, dict)

loop_row :: Int->Int->[[(Int, Int)]]-> Map (Int, Int) Int -> Map(Int, Int) Int
loop_row i n rows dict = do
   if i == n then dict else do
      let curr_row = (rows !! (i - 1))
      let (edge, x) = curr_row !! 0
      let (_, _, new_dict) = foldl(\v r -> scan_row v r) (edge, i, dict) (init curr_row)
      loop_row (i + 1) n rows new_dict

t_item :: ((Int, Int), Int) -> ((Int, Int), Int)
t_item item = do
    let ((r, c), v) = item
    ((c, r), v)

parse_to_rows input_list = do
    let r0 = map(\row -> (Text.unpack row)) input_list
    map(\row -> (map(\s -> (read (s:[]) :: Int)) row)) r0

ms_helper :: (Int, Bool)->Int->Int -> (Int, Bool)
ms_helper n h my_h = do
   let (max, seen) = n
   if seen == False && h < my_h then
       (max + 1, seen)
   else if seen == False then
       (max + 1, True)
   else (max, seen)

max_seen_trees :: [Int]->Int
max_seen_trees list = do
   let my_h = (list !! 0)
   let fr = foldl(\n h -> (ms_helper n h my_h)) (0, False) (tail list)
   fst fr

calc_curr_tree_max :: Int->Int->[[Int]]->Int
calc_curr_tree_max i j grid = do
   let row = grid !! j
   let left = reverse (take (i + 1) row)
   let right = drop i row
   let col = ((transpose grid) !! i)
   let up = reverse (take (j+1) col)
   let down = drop j col
   let result = foldl(*) 1 (map(\x -> max_seen_trees x)(left:right:up:down:[]))
   result

loop_map :: Int->Int->Int->Int->Int->[[Int]]-> Int
loop_map i j max_i max_j prev_max grid = do
   if i < max_i && j < max_j then do
      let v = calc_curr_tree_max i j grid
      let curr_max = max v prev_max
      loop_map (i+1) j max_i max_j curr_max grid
   else do
      if j < max_j then do
        loop_map 0 (j+1) max_i max_j prev_max grid
      else do prev_max

part1 input = do
    let r1 = input
    let r1_t = transpose input
    let edges = ((length r1) + ((length (r1 !! 0) - 2))) * 2
    let r2 = tail (init(map(\row -> zip row [0..]) r1))
    let r3 = tail (init(map(\row -> reverse (zip row [0..])) r1))
    let r4 = tail (init(map(\row -> zip row [0..]) r1_t))
    let r5 = tail (init(map(\row -> reverse (zip row [0..])) r1_t))

    let dict :: Map (Int, Int) Int = Map.fromList[]
    let dict1 = loop_row 1 ((length r2) + 1) r2 dict
    let dict2 = loop_row 1 ((length r3) + 1) r3 dict1
    let dict_t :: Map (Int, Int) Int = Map.fromList[]
    let dict1_t = loop_row 1 ((length r4) + 1) r4 dict_t
    let dict2_t = loop_row 1 ((length r5) + 1) r5 dict1_t

    let dict2_tt = Map.fromList (map(\item -> t_item item)(Map.toList dict2_t))
    let result_dict = Map.union dict2 dict2_tt
    let inner = Map.size result_dict
    print (edges + inner)

part2 input = do
    let max_j = length input
    let max_i = length (input !! 0)
    let result = loop_map 0 0 max_i max_j 0 input
    print result

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day8.txt")
    let input = parse_to_rows input_list
    part1 input
    part2 input
