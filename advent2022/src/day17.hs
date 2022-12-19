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



brick_points sx sy i
    | i == 0 = [(sx, sy), (sx+1, sy), (sx+2, sy), (sx+3, sy)]
    | i == 1 = [(sx+1, sy), (sx, sy-1), (sx+1, sy-1), (sx+2, sy-1), (sx+1, sy-2)]
    | i == 2 = [(sx+2, sy), (sx+2, sy-1), (sx, sy-2), (sx+1, sy-2), (sx+2, sy-2)]
    | i == 3 = [(sx, sy), (sx, sy-1), (sx, sy-2), (sx, sy-3)]
    | i == 4 = [(sx, sy), (sx+1, sy), (sx, sy-1), (sx+1, sy-1)]

brick_dims i
    | i == 0 = (4, 1)
    | i == 1 = (3, 3)
    | i == 2 = (3, 3)
    | i == 3 = (1, 4)
    | i == 4 = (2, 2)

display_helper i length well = do
    if i == length then do
        print "+-------+"
    else do
        print ("|" ++ (well !! i) ++ "|")
        display_helper (i + 1) length well

display well = do
    display_helper 0 (length well) (reverse well)

get_p x y well = do
    if y >= (length well) then '.' else do
       if y < 0 then '@' else ((well !! y) !! x)


update_well_point x y well c = do
    let row = well !! y
    let new_row = (take x row) ++ [c] ++ (drop (x + 1) row)
    (take y well) ++ [new_row] ++ (drop (y + 1) well)


update_well well sx sy brick_idx = do
    let bp = brick_points sx sy brick_idx
    let well_h = length well
    let well1 = if (sy >= well_h) then well ++ (take (sy - well_h + 1) (repeat ".......")) else well
    foldl(\w (x, y) -> update_well_point x y w '#' ) well1 bp

step_brick brick_pos brick_idx well patterns pi well_width well_height = do
    let (sx_in, sy_in) = brick_pos
    let (brick_w, brick_h) = brick_dims brick_idx
    let sx_left = if sx_in == 0 then sx_in else sx_in - 1
    let sx_right = if sx_in + brick_w == well_width then sx_in else sx_in + 1
    let m_pi = pi `mod` (length patterns)
    let pattern = patterns !! m_pi
    let sx = if pattern == '<' then sx_left else sx_right
    let sy = sy_in
    let brick = brick_points sx sy brick_idx
    let can_move = all (\v -> v == '.') (map (\(i, j) -> get_p i j well) brick)
    let sx1 = if can_move then sx else sx_in
    let sy1 = sy_in - 1
    let brick1 = brick_points sx1 sy1 brick_idx
    let can_drop = all (\v -> v == '.') (map (\(i, j) -> get_p i j well) brick1)
    if can_drop then
        step_brick (sx1, sy1) brick_idx well patterns (pi + 1) well_width well_height
    else
       ((sx1, sy_in), pi + 1)

drop_brick brick_i well pattern_i patterns well_w = do
    let well_h = length well
    let (bw, bh) = brick_dims brick_i
    let ((bx, by), pi) = step_brick (2, well_h + 2 + bh) brick_i well patterns pattern_i well_w (length well)
    ((bx, by), pi, (2, well_h + 2 + bh))

play_part1 i steps patterns pi well w = do
    if i == steps then (well, pi) else do
        let bi = i `mod` 5
        let ((bx, by), new_pi, (_, _)) = drop_brick bi well pi patterns w
        let new_well = update_well well bx by bi
        play_part1 (i + 1) steps patterns new_pi new_well w

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day17_test.txt")
    --part1 input_list
    part2 input_list

part1 input_list = do
    let patterns = (head (map(\line -> (Text.unpack line)) input_list))
    let well = []
    let steps = 2022
    let (r3, r4) = play_part1 0 steps patterns 0 well 7
    print (length r3)


find_v elem hmap = case Map.lookup elem hmap of
    Nothing -> -1
    Just s -> s

play_part2 i steps patterns pi well w hmap = do
    if i == steps then (well, pi, hmap, i, steps, -1) else do
        let bi = i `mod` 5
        let ((bx, by), new_pi, (ix, iy)) = drop_brick bi well pi patterns w

        let dx = (ix - bx)
        let dy = (iy - by)
        let new_well = update_well well bx by bi
        let v = find_v ((dx, dy), bi, pi) hmap
        -- let v = -1
        if v /= -1 then (well, pi, hmap, i, steps, v) else do
            let new_map = Map.insert ((dx, dy), bi, pi) i hmap
            play_part2 (i + 1) steps patterns new_pi new_well w new_map


part2 input_list = do
    let patterns = (head (map(\line -> (Text.unpack line)) input_list))
    let well = []
    --display well

    --let r1 = drop_brick 2 well 8 patterns 7
    --print r1

    --let ((x, y), p) = r1
    --let r2 = update_well well x y 0
    --display r2

    --let steps = 10
    -- let (r3, r4) = play 0 steps patterns 0 well 7
    --display r3
    --print (length r3)
    --print (last r3)
    --print r4

    let steps = 500
    let (r3, r4) = play_part1 0 steps patterns 0 well 7

    let hmap :: Map ((Int, Int), Int, Int) Int = Map.fromList[]
    -- let (r_well, pi, new_map, i, steps, v) = play_part2 0 2000 patterns 0 well 7 hmap
    let (r_well, pi, new_map, i, steps, v) = play_part2 0 20000 patterns 0 r3 7 hmap
    print pi
    print i
    print steps
    print v
    -- print new_map

