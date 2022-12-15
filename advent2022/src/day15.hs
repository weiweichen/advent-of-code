import System.IO
import Data.List as List
import Data.List.Split as Split
import Data.List (sort)
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

parse_sb_input line = do
    let s_in = line !! 0
    let b_in = line !! 1
    let s_xy = Split.splitOn ", " (drop (length "Sensor at ") s_in)
    let b_xy = Split.splitOn ", " (drop (length "closest beacon is at ") b_in)
    let sx = read (drop 2 (s_xy !! 0)) :: Integer
    let sy = read (drop 2 (s_xy !! 1)) :: Integer
    let bx = read (drop 2 (b_xy !! 0)) :: Integer
    let by = read (drop 2 (b_xy !! 1)) :: Integer
    ((sx, sy), (bx, by))

m_distance sb = do
    let ((sx, sy), (bx, by)) = sb
    (abs (sx - bx)) + (abs (sy - by))

calc_range sb y = do
    let (((sx, sy), (_, _)),md) = sb
    let dy = abs (sy - y)
    if (dy > md) then (-4000001, -4000001) else do
        let dx_max = md - dy
        (sx - dx_max, sx + dx_max)

fold_merge curr_range result = do
    let (cs, ce) = curr_range
    if (length result) == 0 then [curr_range] else do
        let (ps, pe) = last result
        if ce <= pe then result else do
            if cs <= pe + 1 then (init result) ++ [(ps, ce)] else do
                result ++ [curr_range]

merge_ranges ranges = do
    foldl(\result r -> fold_merge r result) [] ranges

count_points range result = do
    let (s, e) = range
    result + (e - s + 1)

is_in_range ranges p = do
    foldl(\acc (s, e) -> if p >= s && p <= e then True else acc) False ranges

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day15.txt")
    part1 input_list
    part2 input_list

part1 input_list = do
    let r0 = map(\line -> Split.splitOn ": " (Text.unpack line)) input_list
    let r1 = map(\line -> parse_sb_input line) r0
    let r2 = map(\sb -> (sb, (m_distance sb))) r1
    let beams = Set.toList (Set.fromList (map(\sb -> snd sb) r1))
    let row = 2000000
    let r3 = sort (filter(\s -> s /= (-4000001, -4000001)) (map(\sbm -> calc_range sbm row) r2))
    let r4 = foldl(\result r -> fold_merge r result) [] r3
    let r5 = foldl(\acc r -> count_points r acc) 0 r4
    let r6 = foldl(\acc (bx, by) -> if by == row && is_in_range r4 bx then acc - 1 else acc) r5 beams
    print r6

part2 input_list = do
    let r0 = map(\line -> Split.splitOn ": " (Text.unpack line)) input_list
    let r1 = map(\line -> parse_sb_input line) r0
    let r2 = map(\sb -> (sb, (m_distance sb))) r1
    let beams = Set.toList (Set.fromList (map(\sb -> snd sb) r1))
    let r3 = map(\row -> (row, sort (filter(\s -> s /= (-4000001, -4000001)) (map(\sbm -> calc_range sbm row) r2)))) [0..4000000]
    let r4 = map(\(row, ranges) -> (row, merge_ranges ranges)) r3
    let r5 = filter(\(row, ranges) -> (length ranges) > 1) r4
    print r5
    let result = r5 !! 0
    let ry = fst result
    let rx = snd ((snd result) !! 0) + 1
    print (rx * 4000000 + ry)

