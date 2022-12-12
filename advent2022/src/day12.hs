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


locate_helper letter row = case findIndex letter row of
                     Nothing  -> -1
                     Just s -> s

locate :: Char->[String]->(Int, Int)
locate letter hmap = do
    let r0 = map (\row -> locate_helper (\x -> x == letter) row) hmap
    let y = locate_helper (\x -> x /= -1) r0
    ((r0 !! y), y)

get_neighbour_helper curr_p dx dy hmap w h= do
    let (x, y) = curr_p
    let nx = x + dx
    let ny = y + dy
    let letter = if nx < 0 || ny < 0 || nx >= w || ny >= h then 'X' else (hmap !! ny) !! nx
    (letter, (nx, ny))

get_neighbours curr_p width height hmap = do
    let dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    let r0 = map(\dd -> get_neighbour_helper curr_p (fst dd) (snd dd) hmap width height) dirs
    filter(\x -> (fst x) /= 'X' && (fst x) /= 'S') r0

find_cost p cost_map = case Map.lookup p cost_map of
                 Nothing  -> -1
                 Just s -> s

update_neighbours :: Int->Int->[(Int, Int)]->Map (Int, Int) Int-> Char -> Int -> [(Char, (Int, Int))]-> (Map (Int, Int) Int, [(Int, Int)])
update_neighbours i n queue cost_map curr_h curr_cost neighbours = do
    if i == n then (cost_map, queue) else do
        let neighbour = neighbours !! i
        let (nv, (nx, ny)) = neighbour
        let curr_rh = if curr_h == 'S' then 'a' else curr_h
        let n_rv = if nv == 'E' then 'z' else nv
        let dv = (fromEnum n_rv) - (fromEnum curr_rh)

        if dv > 1 then update_neighbours (i + 1) n queue cost_map curr_h curr_cost neighbours else do
            let n_cost = find_cost (nx, ny) cost_map
            let new_map = Map.insert (nx, ny) (curr_cost + 1) cost_map
            let new_queue = if nv == 'E' then queue else queue ++ [(nx, ny)]
            if n_cost == -1 || n_cost > curr_cost + 1 then update_neighbours (i + 1) n new_queue new_map curr_h curr_cost neighbours
            else update_neighbours (i + 1) n queue cost_map curr_h curr_cost neighbours

shortest_path :: (Int, Int)->(Int, Int)->[(Int, Int)]-> [String] -> Map (Int, Int) Int -> Int -> Int -> Map (Int, Int) Int
shortest_path start_p end_p queue hmap cost_map w h = do
    if (length queue) == 0 then cost_map else do
        let (curr_x, curr_y) = head queue
        let curr_v = (hmap !! curr_y) !! curr_x
        let curr_cost = find_cost (curr_x, curr_y) cost_map
        let queue_tail = tail queue
        let curr_h = (hmap !! curr_y) !! curr_x
        let neighbours = get_neighbours (curr_x, curr_y) w h hmap
        let (new_cost_map, new_queue) = update_neighbours 0 (length neighbours) queue_tail cost_map curr_v curr_cost neighbours
        shortest_path start_p end_p new_queue hmap new_cost_map w h

solve start_p end_p hmap w h = do
    let cost_map :: Map (Int, Int) Int = Map.fromList [(start_p, 0)]
    let work_q = [start_p]
    let new_cost_map = shortest_path start_p end_p work_q hmap cost_map w h
    find_cost end_p new_cost_map

find_letters i j letter hmap w h result = do
    if i == w && j == h - 1 then result else do
        if i == w && j < h - 1 then find_letters 0 (j + 1) letter hmap w h result else do
            let curr_h = (hmap !! j) !! i
            let new_result = if curr_h == letter then result ++ [(i, j)] else result
            find_letters (i + 1) j letter hmap w h new_result

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day12.txt")
    let hmap = map(\line -> Text.unpack line) input_list
    let w = length (hmap !! 0)
    let h = length hmap
    part1 hmap w h
    part2 hmap w h

part1 hmap w h = do
    let pos_s = locate 'S' hmap
    let pos_e = locate 'E' hmap
    let cost = solve pos_s pos_e hmap w h
    print cost

part2 hmap w h = do
    let pos_s = locate 'S' hmap
    let pos_e = locate 'E' hmap
    let start_ps = find_letters 0 0 'a' hmap w h []
    let result = filter(\p -> (fst p) /= -1)(map(\p -> ((solve p pos_e hmap w h), p)) ([pos_s] ++ start_ps))
    print (fst (minimum result))
