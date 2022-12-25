import System.IO
import Data.List as List
import Data.List.Split as Split
import Data.List (sort)
import Data.Bits
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Typeable
import Data.Set (toList, fromList)
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names
                                  -- prefixed with "Map." (with the period).
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Loops
import Control.Monad
import Control.Monad.IO.Class
import Data.Ix (inRange)


print_grid i grid = do
   if i == (length grid) then do
        print " "
   else do
        print (grid !! i)
        print_grid (i + 1) grid

parse_inst inst_input = do
    let r1 = Split.splitOn "R" inst_input
    let r2 = insert_ch "R" r1
    let r3 = map(\item -> parse_item item) r2
    flatten_list r3

insert_ch ch [x] = [x]
insert_ch ch (x:xs) = [x, ch] ++ (insert_ch ch xs)

parse_item item = do
    if item == "R" then [item] else do
        let r0 = Split.splitOn "L" item
        (insert_ch "L" r0) ++ (if (last item) == 'L' then ["L"] else [])

flatten_list [] = []
flatten_list ((i:is):xs) = [i] ++ flatten_list (is:xs)
flatten_list ([]:xs) = flatten_list xs

get_dir dxy dir
    | dir == "L" && dxy == (0, 1) = (1, 0)
    | dir == "L" && dxy == (0, -1) = (-1, 0)
    | dir == "L" && dxy == (1, 0) = (0, -1)
    | dir == "L" && dxy == (-1, 0) = (0, 1)
    | dir == "R" && dxy == (0, 1) = (-1, 0)
    | dir == "R" && dxy == (0, -1) = (1, 0)
    | dir == "R" && dxy == (1, 0) = (0, 1)
    | dir == "R" && dxy == (-1, 0) = (0, -1)


next_pos curr_pos dir grid = do
    let (curr_x, curr_y) = curr_pos
    let (dx, dy) = dir
    let (next_x, next_y) = (curr_x + dx, (mod (curr_y + dy) (length grid)))
    let w = length (grid !! next_y)
    let (new_next_x, new_next_y) = ((mod next_x w), next_y)
    let point = (grid !! new_next_y) !! new_next_x
    if point == ' ' then do
        let (nx, ny) = next_pos (new_next_x, new_next_y) dir grid
        let point = (grid !! ny) !! nx
        if point == ' ' then curr_pos else (nx, ny)
    else do
        if point == '#' then curr_pos else (new_next_x, new_next_y)

facing_value dir
    | dir == (1, 0) = 0
    | dir == (-1, 0) = 2
    | dir == (0, 1) = 1
    | dir == (0, -1) = 3

move_dir curr_pos curr_s steps grid dir = do
    if curr_s == steps then curr_pos else do
        let (next_x, next_y) = next_pos curr_pos dir grid
        if curr_pos == (next_x, next_y) then curr_pos else
            move_dir (next_x, next_y) (curr_s + 1) steps grid dir

move_one_step x y insts grid nth n_actions dir = do
   --if (nth == n_actions) then (x, y, q) else do
   if (nth == n_actions) then (x, y, dir) else do
      let action = (insts !! nth)
      if action == "L" || action == "R" then do
         let new_dir = get_dir dir action
         move_one_step x y insts grid (nth + 1) n_actions new_dir
      else do
         let steps :: Int = read action
         let (next_x, next_y) = move_dir (x, y) 0 steps grid dir
         move_one_step next_x next_y insts grid (nth + 1) n_actions dir


{-
-- face 1
next_pos_part2 (x, y) 1 (0, -1) cube_size =
    if y == 0 then ((cube_size - x - 1, 0), 2, (0, 1)) else next_pos_part2_help (x, y) 1 (0, -1) cube_size

next_pos_part2 (x, y) 1 (0, 1) cube_size =
    if y == cube_size - 1 then ((x, 0), 4, (0, 1)) else next_pos_part2_help (x, y) 1 (0, 1) cube_size

next_pos_part2 (x, y) 1 (-1, 0) cube_size =
    if x == 0 then ((y, 0), 3, (0, 1)) else next_pos_part2_help (x, y) 1 (-1, 0) cube_size

next_pos_part2 (x, y) 1 (1, 0) cube_size =
    if x == cube_size - 1 then ((cube_size - 1, cube_size - 1 - y), 6, (-1, 0)) else next_pos_part2_help (x, y) 1 (1, 0) cube_size

-- face 2
next_pos_part2 (x, y) 2 (0, -1) cube_size =
    if y == 0 then ((cube_size - x - 1, 0), 1, (0, 1)) else next_pos_part2_help (x, y) 2 (0, -1) cube_size

next_pos_part2 (x, y) 2 (0, 1) cube_size =
    if y == cube_size - 1 then ((cube_size -1 - x, cube_size - 1), 5, (0, -1)) else next_pos_part2_help (x, y) 2 (0, 1) cube_size

next_pos_part2 (x, y) 2 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1 - y, cube_size - 1), 6, (0, -1)) else next_pos_part2_help (x, y) 2 (-1, 0) cube_size

next_pos_part2 (x, y) 2 (1, 0) cube_size =
    if x == cube_size - 1 then ((0, y), 3, (1, 0)) else next_pos_part2_help (x, y) 2 (1, 0) cube_size

-- face 3
next_pos_part2 (x, y) 3 (0, -1) cube_size =
    if y == 0 then ((0, x), 1, (0, 1)) else next_pos_part2_help (x, y) 3 (0, -1) cube_size

next_pos_part2 (x, y) 3 (0, 1) cube_size =
    if y == cube_size - 1 then ((0, cube_size -1 - x), 5, (0, 1)) else next_pos_part2_help (x, y) 3 (0, 1) cube_size

next_pos_part2 (x, y) 3 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1, y), 2, (-1, 0)) else next_pos_part2_help (x, y) 3 (-1, 0) cube_size

next_pos_part2 (x, y) 3 (1, 0) cube_size =
     if x == cube_size - 1 then ((0, y), 4, (1, 0)) else next_pos_part2_help (x, y) 3 (1, 0) cube_size


-- face 4
next_pos_part2 (x, y) 4 (0, -1) cube_size =
    if y == 0 then ((x, cube_size - 1), 1, (0, -1)) else next_pos_part2_help (x, y) 4 (0, -1) cube_size

next_pos_part2 (x, y) 4 (0, 1) cube_size =
    if y == cube_size - 1 then ((x, 0), 5, (0, 1)) else next_pos_part2_help (x, y) 4 (0, 1) cube_size

next_pos_part2 (x, y) 4 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1, y), 3, (-1, 0)) else next_pos_part2_help (x, y) 4 (-1, 0) cube_size

next_pos_part2 (x, y) 4 (1, 0) cube_size =
    if x == cube_size - 1 then ((cube_size - 1 - y, 0), 6, (0, 1)) else next_pos_part2_help (x, y) 4 (1, 0) cube_size

-- face 5
next_pos_part2 (x, y) 5 (0, -1) cube_size =
    if y == 0 then ((x, cube_size - 1), 4, (0, -1)) else next_pos_part2_help (x, y) 5 (0, -1) cube_size

next_pos_part2 (x, y) 5 (0, 1) cube_size =
    if y == cube_size - 1 then ((cube_size -1 - x, cube_size - 1), 2, (0, -1)) else next_pos_part2_help (x, y) 5 (0, 1) cube_size

next_pos_part2 (x, y) 5 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1 - y, cube_size - 1), 3, (0, -1)) else next_pos_part2_help (x, y) 5 (-1, 0) cube_size

next_pos_part2 (x, y) 5 (1, 0) cube_size =
    if x == cube_size - 1 then ((0, y), 6, (1, 0)) else next_pos_part2_help (x, y) 5 (1, 0) cube_size

-- face 6
next_pos_part2 (x, y) 6 (0, -1) cube_size =
    if y == 0 then ((cube_size - 1, cube_size - x - 1), 4, (-1, 0)) else next_pos_part2_help (x, y) 6 (0, -1) cube_size

next_pos_part2 (x, y) 6 (0, 1) cube_size =
    if y == cube_size - 1 then ((0, cube_size -1 - x), 2, (1, 0)) else next_pos_part2_help (x, y) 6 (0, 1) cube_size

next_pos_part2 (x, y) 6 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1, y), 5, (-1, 0)) else next_pos_part2_help (x, y) 6 (-1, 0) cube_size

next_pos_part2 (x, y) 6 (1, 0) cube_size =
     if x == cube_size - 1 then ((cube_size - 1, cube_size - 1 - y), 1, (-1, 0)) else next_pos_part2_help (x, y) 6 (1, 0) cube_size
-}


-- face 1
next_pos_part2 (x, y) 1 (0, -1) cube_size =
    if y == 0 then ((0, x), 6, (1, 0)) else next_pos_part2_help (x, y) 1 (0, -1) cube_size

next_pos_part2 (x, y) 1 (0, 1) cube_size =
    if y == cube_size - 1 then ((x, 0), 3, (0, 1)) else next_pos_part2_help (x, y) 1 (0, 1) cube_size

next_pos_part2 (x, y) 1 (-1, 0) cube_size =
    if x == 0 then ((0, cube_size - 1 - y), 4, (1, 0)) else next_pos_part2_help (x, y) 1 (-1, 0) cube_size

next_pos_part2 (x, y) 1 (1, 0) cube_size =
    if x == cube_size - 1 then ((0, y), 2, (1, 0)) else next_pos_part2_help (x, y) 1 (1, 0) cube_size

-- face 2
next_pos_part2 (x, y) 2 (0, -1) cube_size =
    if y == 0 then ((x, cube_size - 1), 6, (0, -1)) else next_pos_part2_help (x, y) 2 (0, -1) cube_size

next_pos_part2 (x, y) 2 (0, 1) cube_size =
    if y == cube_size - 1 then ((cube_size -1,  x), 3, (-1, 0)) else next_pos_part2_help (x, y) 2 (0, 1) cube_size

next_pos_part2 (x, y) 2 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1, y), 1, (-1, 0)) else next_pos_part2_help (x, y) 2 (-1, 0) cube_size

next_pos_part2 (x, y) 2 (1, 0) cube_size =
    if x == cube_size - 1 then ((cube_size - 1, cube_size - y - 1), 5, (-1, 0)) else next_pos_part2_help (x, y) 2 (1, 0) cube_size

-- face 3
next_pos_part2 (x, y) 3 (0, -1) cube_size =
    if y == 0 then ((x, cube_size - 1), 1, (0, -1)) else next_pos_part2_help (x, y) 3 (0, -1) cube_size

next_pos_part2 (x, y) 3 (0, 1) cube_size =
    if y == cube_size - 1 then ((x, 0), 5, (0, 1)) else next_pos_part2_help (x, y) 3 (0, 1) cube_size

next_pos_part2 (x, y) 3 (-1, 0) cube_size =
    if x == 0 then ((y, 0), 4, (0, 1)) else next_pos_part2_help (x, y) 3 (-1, 0) cube_size

next_pos_part2 (x, y) 3 (1, 0) cube_size =
     if x == cube_size - 1 then ((y, cube_size - 1), 2, (0, -1)) else next_pos_part2_help (x, y) 3 (1, 0) cube_size


-- face 4
next_pos_part2 (x, y) 4 (0, -1) cube_size =
    if y == 0 then ((0, x), 3, (1, 0)) else next_pos_part2_help (x, y) 4 (0, -1) cube_size

next_pos_part2 (x, y) 4 (0, 1) cube_size =
    if y == cube_size - 1 then ((x, 0), 6, (0, 1)) else next_pos_part2_help (x, y) 4 (0, 1) cube_size

next_pos_part2 (x, y) 4 (-1, 0) cube_size =
    if x == 0 then ((0, cube_size - 1 - y), 1, (1, 0)) else next_pos_part2_help (x, y) 4 (-1, 0) cube_size

next_pos_part2 (x, y) 4 (1, 0) cube_size =
    if x == cube_size - 1 then ((0, y), 5, (1, 0)) else next_pos_part2_help (x, y) 4 (1, 0) cube_size

-- face 5
next_pos_part2 (x, y) 5 (0, -1) cube_size =
    if y == 0 then ((x, cube_size - 1), 3, (0, -1)) else next_pos_part2_help (x, y) 5 (0, -1) cube_size

next_pos_part2 (x, y) 5 (0, 1) cube_size =
    if y == cube_size - 1 then ((cube_size -1, x), 6, (-1, 0)) else next_pos_part2_help (x, y) 5 (0, 1) cube_size

next_pos_part2 (x, y) 5 (-1, 0) cube_size =
    if x == 0 then ((cube_size - 1, y), 4, (-1, 0)) else next_pos_part2_help (x, y) 5 (-1, 0) cube_size

next_pos_part2 (x, y) 5 (1, 0) cube_size =
    if x == cube_size - 1 then ((cube_size - 1, cube_size - 1 - y), 2, (-1, 0)) else next_pos_part2_help (x, y) 5 (1, 0) cube_size

-- face 6
next_pos_part2 (x, y) 6 (0, -1) cube_size =
    if y == 0 then ((x, cube_size - 1), 4, (0, -1)) else next_pos_part2_help (x, y) 6 (0, -1) cube_size

next_pos_part2 (x, y) 6 (0, 1) cube_size =
    if y == cube_size - 1 then ((x, 0), 2, (0, 1)) else next_pos_part2_help (x, y) 6 (0, 1) cube_size

next_pos_part2 (x, y) 6 (-1, 0) cube_size =
    if x == 0 then ((y, 0), 1, (0, 1)) else next_pos_part2_help (x, y) 6 (-1, 0) cube_size

next_pos_part2 (x, y) 6 (1, 0) cube_size =
     if x == cube_size - 1 then ((y, cube_size - 1), 5, (0, -1)) else next_pos_part2_help (x, y) 6 (1, 0) cube_size


next_pos_part2_help curr_pos curr_face curr_dir cube_size = do
    let (x, y) = curr_pos
    let (dx, dy) = curr_dir
    ((x + dx, y + dy), curr_face, curr_dir)


move_dir_part2 curr_pos curr_s steps grid dir curr_face cube_size = do
    if curr_s == steps then (curr_pos, curr_face, dir) else do
        let ((next_x, next_y), next_face, next_dir) = next_pos_part2 curr_pos curr_face dir cube_size
        let next_p = ((grid !! (next_face - 1)) !! next_y) !! next_x
        if next_p == '#' then (curr_pos, curr_face, dir) else
            move_dir_part2 (next_x, next_y) (curr_s + 1) steps grid next_dir next_face cube_size


move_part2 x y insts grid nth n_actions dir face cube_size = do
   if (nth == n_actions) then ((x, y), face, dir) else do
      let action = (insts !! nth)
      if action == "L" || action == "R" then do
         let new_dir = get_dir dir action
         move_part2 x y insts grid (nth + 1) n_actions new_dir face cube_size
      else do
         let steps :: Int = read action
         let ((next_x, next_y), next_face, next_dir) = move_dir_part2 (x, y) 0 steps grid dir face cube_size
         move_part2 next_x next_y insts grid (nth + 1) n_actions next_dir next_face cube_size



get_inputs face cube_size = [
     ((0, 0), face, (0, -1)),
     ((0, 0), face, (-1, 0)),
     ((0, cube_size-1), face, (-1, 0)),
     ((0, cube_size-1), face, (0, 1)),
     ((cube_size-1, 0), face, (1, 0)),
     ((cube_size-1, 0), face, (0, -1)),
     ((cube_size-1, cube_size-1), face, (1, 0)),
     ((cube_size-1, cube_size-1), face, (0, 1))]


main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day22.txt")
    let r0 = Split.splitOn (Text.pack(""):[]) input_list
    let grid = map(\line -> Text.unpack line)(r0 !! 0)
    let inst_input = head (map(\line -> Text.unpack line)(r0 !! 1))
    let insts = parse_inst inst_input
    -- part1 grid insts
    part2 grid insts

part1 grid insts = do
    let (col, row, dir) = move_one_step 0 0 insts grid 0 (length insts) (1, 0)
    let res = (col + 1) * 4 + (row + 1) * 1000 + (facing_value dir)
    print res


part2 grid insts = do
    print (length insts)
    let cube_size = 50
    -- hard code the face input faces here based on input file
    let f1 = map(\line -> take cube_size (drop cube_size line)) (take cube_size grid)
    let f2 = map(\line -> take cube_size (drop (cube_size * 2) line)) (take cube_size grid)
    let f3 = map(\line -> take cube_size (drop (cube_size * 1) line)) (take cube_size (drop cube_size grid))

    let f4 = map(\line -> take cube_size line) (take cube_size (drop (cube_size * 2) grid))
    let f5 = map(\line -> take cube_size (drop (cube_size * 1) line)) (take cube_size (drop (cube_size * 2) grid))
    let f6 = map(\line -> take cube_size line) (take cube_size (drop (cube_size * 3) grid))

    let r1 = move_part2 0 0 insts [f1, f2, f3, f4, f5, f6] 0 (length insts) (1, 0) 1 cube_size
    print r1

    let ((col, row), face, dir) = r1
    -- I know the res is on face 2, so hard code the equation here
    let res = (col + 100 + 1) * 4 + (row + 1) * 1000 + (facing_value dir)
    print res

