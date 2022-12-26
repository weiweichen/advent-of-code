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


has_neighbors pos set dirs = do
    let (x, y) = pos
    foldl(\r (dx, dy) -> ((Set.member (x + dx, y + dy) set) || r)) False dirs

all_eight = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
north = ([(0, -1), (-1, -1), (1, -1)], (0, -1))
south = ([(0, 1), (-1, 1), (1, 1)], (0, 1))
west = ([(-1, 0), (-1, -1), (-1, 1)], (-1, 0))
east = ([(1, 0), (1, -1), (1, 1)], (1, 0))

next_dir pos i priority set = do
    if i == (length priority) then pos else do
        let (x, y) = pos
        let (dirs, (dx, dy)) = (priority !! i)
        if (not (has_neighbors pos set dirs)) then ((x + dx), (y + dy)) else
            next_dir pos (i + 1) priority set

next_pos pos priority set = do
    if (not (has_neighbors pos set all_eight)) then pos else
        next_dir pos 0 priority set

find_key key map = case Map.lookup key map of
    Nothing -> []
    Just s -> s

update_map elves set priority map i = do
    if (i == length elves) then map else do
        let (x, y) = (elves !! i)
        let (nx, ny) = next_pos (x, y) priority set
        let v = find_key (nx, ny) map
        let new_map = if (length v) == 0  then Map.insert (nx, ny) [(x, y)] map else Map.insert (nx, ny) (v ++ [(x, y)]) map
        update_map elves set priority new_map (i + 1)

move set priority i steps = do
    if i == steps then (set, priority) else do
        let elves = (Set.toList set)
        let new_map = update_map elves set priority (Map.fromList[]) 0
        --let r0 = filter(\(pos, v) -> v == 1) (Map.toList new_map)
        let r0 = (Map.toList new_map)
        let r1 = filter(\(pos, v) -> (length v) == 1) r0
        let r2 = filter(\(pos, v) -> (length v) /= 1) r0
        let r3 = map(\pair -> fst pair) r1
        let r4 = foldl(\list (p, v) -> list ++ v) r3 r2
        let new_set = Set.fromList r4
        let new_priority = (drop 1 priority) ++ [head priority]
        move new_set new_priority (i + 1) steps


move_part2 set priority i = do
    let elves = (Set.toList set)
    let new_map = update_map elves set priority (Map.fromList[]) 0
    let r0 = (Map.toList new_map)
    let r1 = filter(\(pos, v) -> (length v) == 1) r0
    let r2 = filter(\(pos, v) -> (length v) /= 1) r0
    let r3 = map(\pair -> fst pair) r1
    let r4 = foldl(\list (p, v) -> list ++ v) r3 r2
    let new_set = Set.fromList r4
    let new_priority = (drop 1 priority) ++ [head priority]
    if (sort (Set.toList set)) == (sort r4) then i else
        move_part2 new_set new_priority (i + 1)



main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day23.txt")
    let inputs = map(\line -> Text.unpack line) input_list
    let r0 =  foldl(\s (i, j) -> if ((inputs !! j) !! i) == '#' then Set.insert (i, j) s else s)
              (Set.fromList[]) ([(i, j) | i <- [0 .. ((length (inputs !! 0)) - 1)], j <- [0 .. ((length inputs) - 1)]])
    let pr = [north, south, west, east]
    part1 r0 pr
    part2 r0 pr

part1  r0 pr = do
    let (r1, p) = move r0 pr 0 10

    let r2 = map(\p -> fst p) (Set.toList r1)
    let r3 = map(\p -> snd p) (Set.toList r1)
    let max_x = maximum r2
    let min_x = minimum r2
    let max_y = maximum r3
    let min_y = minimum r3
    let res = (max_x - min_x + 1) * (max_y - min_y + 1) - (length r1)
    print res

part2 r0 pr = do
    let res = move_part2 r0 pr 1
    print res

