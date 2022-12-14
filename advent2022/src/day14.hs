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

shift coord x_offset = do
    let (x, y)= coord
    (x - x_offset + 1, y)

show_graph i graph = do
    if i == (length graph) then do
        print (take (length (graph !! 0)) (repeat '-'))
    else do
        print ((graph !! i) ++ (show i))
        show_graph (i + 1) graph

draw_point x y graph c = do
    let g1 = take y graph
    let g2 = drop (y+1) graph
    let l = graph !! y
    let new_l = (take x l) ++ c ++ (drop (x + 1) l)
    g1 ++ [new_l] ++ g2

draw_vline y y2 x graph c = do
    if y == y2 + 1 then graph else do
        let next_graph = draw_point x y graph c
        draw_vline (y + 1) y2 x next_graph c

draw_hline x x2 y graph c = do
     if x == x2 + 1 then graph else do
         let next_graph = draw_point x y graph c
         draw_hline (x + 1) x2 y next_graph c

draw_p2p p1 p2 graph c = do
    let (x1, y1) = p1
    let (x2, y2) = p2
    if x1 == x2 then do
        -- draw vertical line
        if y1 < y2 then draw_vline y1 y2 x1 graph c
        else draw_vline y2 y1 x1 graph c
    else do
        -- draw horizontal line
        if x1 < x2 then draw_hline x1 x2 y1 graph c
        else draw_hline x2 x1 y1 graph c


draw_line i line graph c = do
    if i == (length line) then graph else do
        let p1 = line !! (i - 1)
        let p2 = line !! i
        let next_graph = draw_p2p p1 p2 graph c
        draw_line (i + 1) line next_graph c

draw_graph i lines graph c = do
    if i == (length lines) then graph else do
        let line = lines !! i
        let next_graph = draw_line 1 line graph c
        draw_graph (i + 1) lines next_graph c

get_point p graph = do
    let (x, y) = p
    (graph !! y) !! x

get_point_test p graph = do
    let (x, y) = p
    if y >= length graph then 'X' else do
        (graph !! y) !! x

drop_sand sp graph w = do
    let (sx, sy) = sp
    if sx == 0 || sx == w - 1 || sy == ((length graph) - 1) then sp else do
        let (d1, d2, d3) = ((sx, sy + 1), (sx - 1, sy + 1), (sx + 1, sy + 1))
        let p1 = get_point d1 graph
        if p1 == '.' then drop_sand d1 graph w else do
            let p2 = get_point d2 graph
            if p2 == '.' then drop_sand d2 graph w else do
                let p3 = get_point d3 graph
                if p3 == '.' then drop_sand d3 graph w else sp


drop_sand_step i steps sp graph w = do
    let (sx, sy) = sp
    if i == steps || sx == 0 || sx == w - 1 || sy >= ((length graph) - 1)then sp else do
        let (d1, d2, d3) = ((sx, sy + 1), (sx - 1, sy + 1), (sx + 1, sy + 1))
        let p1 = get_point d1 graph
        if p1 == '.' then drop_sand_step (i+1) steps d1 graph w else do
            let p2 = get_point d2 graph
            if p2 == '.' then drop_sand_step (i+1) steps d2 graph w else do
                let p3 = get_point d3 graph
                if p3 == '.' then drop_sand_step (i+1) steps d3 graph w else sp

run_sand_fall i steps sp graph w = do
    if i == steps then graph else do
        let next_sp = drop_sand sp graph w
        let (nx, ny) = next_sp
        let next_graph = draw_point nx ny graph "o"
        run_sand_fall (i + 1) steps sp next_graph w

run_part1 :: Integer->(Int, Int)->[String]->Int->Integer
run_part1 i sp graph w = do
    let (nx, ny) = drop_sand sp graph w
    let next_graph = draw_point nx ny graph "o"
    if nx == 0 || nx == w - 1 || ny >= (length graph) - 1 then i else
        run_part1 (i + 1) sp next_graph w

run_part2_slow :: Integer->(Int, Int)->[String]->Int->Integer
run_part2_slow i sp graph w = do
     let (nx, ny) = drop_sand sp graph w
     if sp == (nx, ny) then i else do
         let next_graph = draw_point nx ny graph "o"
         if nx == 0 || nx == w - 1 || ny >= (length graph) - 1 then i else
             run_part2_slow (i + 1) sp next_graph w

update_queue :: Char->[(Int, Int)]->(Int, Int)->[(Int, Int)]
update_queue p queue sp
    | p == '.' = queue ++ [sp]
    | otherwise = queue

update_graph p graph d
    | p == '.' = draw_point (fst d) (snd d) graph "o"
    | otherwise = graph

run_part2 graph q w = do
    if (length q) == 0 then graph else do
        let (sx, sy) = head q
        let new_graph = draw_point sx sy graph "o"
        let rest_q = tail q
        let (d1, d2, d3) = ((sx, sy + 1), (sx - 1, sy + 1), (sx + 1, sy + 1))
        let p1 = get_point d1 new_graph
        let p2 = get_point d2 new_graph
        let p3 = get_point d3 new_graph
        let q1 = update_queue p1 rest_q d1
        let g1 = update_graph p1 graph d1
        let q2 = update_queue p2 q1 d2
        let g2 = update_graph p2 g1 d2
        let q3 = update_queue p3 q2 d3
        let g3 = update_graph p3 g2 d3
        run_part2 g3 q3 w

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day14.txt")
    let r0 = map(\line -> Text.unpack line) input_list
    let r1 = map(\line -> Split.splitOn " -> " line) r0
    let r2 = map(\coords -> map(\c -> ((read (c!!0) :: Int), (read (c!!1) :: Int)))(map(\c -> Split.splitOn "," c) coords)) r1
    part1 r2
    part2 r2

part1 r2 = do
    let x_coords = map(\line -> map(\p -> fst p) line) r2
    let y_coords = map(\line -> map(\p -> snd p) line) r2
    let min_x = minimum (map(\line -> minimum line) x_coords)
    let max_x = maximum (map(\line -> maximum line) x_coords)
    let min_y = minimum (map(\line -> minimum line) y_coords)
    let max_y = maximum (map(\line -> maximum line) y_coords)
    let r3 = map(\coords -> map(\c -> shift c min_x) coords) r2
    let g0 = take (max_y + 2) (repeat (take (max_x - min_x + 3) (repeat '.')))
    let g1 = draw_graph 0 r3 g0 "#"
    let sand_sp = (500 - min_x + 1, 0)
    let width = length (g1 !! 0)
    let height = length g1
    let result = run_part1 0 sand_sp g1 width
    print result

part2 r2= do
    let x_coords = map(\line -> map(\p -> fst p) line) r2
    let y_coords = map(\line -> map(\p -> snd p) line) r2
    let min_x = minimum (map(\line -> minimum line) x_coords)
    let max_x = maximum (map(\line -> maximum line) x_coords)
    let min_y = minimum (map(\line -> minimum line) y_coords)
    let max_y = maximum (map(\line -> maximum line) y_coords)
    let height = max_y + 3
    let w = maximum([500 - min_x, max_x - 500, height])
    let width = w * 2 + 1
    let r3 = map(\coords -> map(\c -> shift c (500 - w)) coords) r2
    let g0 = take (height - 1) (repeat (take (width) (repeat '.'))) ++ [take width (repeat '#')]
    let g1 = draw_graph 0 r3 g0 "#"
    let sand_sp = (w + 1, 0)
    let g2 = draw_point (w + 1) 0 g1 "o"
    let g3 = run_part2 g2 [sand_sp] width
    -- show_graph 0 g3
    let result = foldl(\acc p -> foldl(\acc1 p1 -> if p1 == 'o' then acc1 + 1 else acc1) acc p) 0 g3
    print result

part2_slow r2= do
    let x_coords = map(\line -> map(\p -> fst p) line) r2
    let y_coords = map(\line -> map(\p -> snd p) line) r2
    let min_x = minimum (map(\line -> minimum line) x_coords)
    let max_x = maximum (map(\line -> maximum line) x_coords)
    let min_y = minimum (map(\line -> minimum line) y_coords)
    let max_y = maximum (map(\line -> maximum line) y_coords)
    let height = max_y + 3
    let w = maximum([500 - min_x, max_x - 500, height])
    let width = w * 2 + 1
    let r3 = map(\coords -> map(\c -> shift c (500 - w)) coords) r2
    let g0 = take (height - 1) (repeat (take (width) (repeat '.'))) ++ [take width (repeat '#')]
    let g1 = draw_graph 0 r3 g0 "#"
    let sand_sp = (w + 1, 0)
    let g2 = run_sand_fall 0 100 sand_sp g1 width
    let result = run_part2_slow 1 sand_sp g1 width
    print result
