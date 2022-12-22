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

parse_line line = do
    let r0 = Split.splitOn ": " line
    let blueprint = read (drop (length "Blueprint ") (r0 !! 0)) :: Int
    let r1 = map(\line -> Split.splitOn " " line)(Split.splitOn ". " (r0 !! 1))
    let ore = read ((r1 !! 0) !! 4) :: Int
    let clay = read ((r1 !! 1) !! 4) :: Int
    let obsidian = ((read ((r1 !! 2) !! 4) :: Int), (read ((r1 !! 2) !! 7) :: Int))
    let geode = ((read ((r1 !! 3) !! 4) :: Int), (read ((r1 !! 3) !! 7) :: Int))
    --(blueprint, ore, clay, obsidian, geode)
    --[[ore, 0, 0, 0], [clay, 0, 0, 0], [(fst obsidian), (snd obsidian), 0, 0], [(fst geode), 0, (snd geode), 0]]
    [(ore, 0, 0, 0), (clay, 0, 0, 0), ((fst obsidian), (snd obsidian), 0, 0), ((fst geode), 0, (snd geode), 0)]

all_gt p1 p2 = do
    let (x0, x1, x2, x3) = p1
    let (y0, y1, y2, y3) = p2
    x0 >= y0 && x1 >= y1 && x2 >= y2 && x3 >= y3

any_gt p1 p2 = do
    any (\(x, y) -> x > y)(take 3 (zip p1 p2))

all_gt_v p1 p2 = do
     let (x0, x1, x2, x3) = (p1 !! 0, p1 !! 1, p1 !! 2, p1 !! 3)
     let (y0, y1, y2, y3) = (p2 !! 0, p2 !! 1, p2 !! 2, p2 !! 3)
     x0 >= y0 && x1 >= y1 && x2 >= y2 && x3 >= y3

sub p1 p2 = do
    let (x0, x1, x2, x3) = p1
    let (y0, y1, y2, y3) = p2
    (x0-y0, x1-y1, x2-y2, x3-y3)

mul k p = do
    let (x0, x1, x2, x3) = p
    (x0 * k, x1 * k, x2 * k, x3 * k)

add :: (Int, Int, Int, Int)->[Int]->(Int, Int, Int, Int)
add p1 p2 = do
    let (x0, x1, x2, x3) = p1
    let (y0, y1, y2, y3) = (p2 !! 0, p2 !! 1, p2 !! 2, p2 !! 3)
    -- let (y0, y1, y2, y3) = p2
    (x0+y0, x1+y1, x2+y2, x3+y3)

max_res_rob cost = do
    let r0 = maximum (map(\(p0, p1, p2, p3)-> p0) cost)
    let r1 = maximum (map(\(p0, p1, p2, p3)-> p1) cost)
    let r2 = maximum (map(\(p0, p1, p2, p3)-> p2) cost)
    let r3 = maximum (map(\(p0, p1, p2, p3)-> p3) cost)
    [r0, r1, r2, r3]

update curr_res curr_robs cost i max_robots = do
    if (curr_robs !! i) >= (max_robots !! i) && i < 3 then (curr_res, curr_robs) else do
        if all_gt curr_res cost then do
            let new_res = sub curr_res cost
            let new_robs = (take i curr_robs) ++ [(curr_robs !! i) + 1] ++ (drop (i + 1) curr_robs)
            (new_res, new_robs)
        else do (curr_res, curr_robs)


get_next_states curr_res curr_robs cost max_robots = do
    let r0 = map(\line -> update curr_res curr_robs (fst line) (snd line) max_robots) (zip cost [0..])
    (filter(\(res, _) -> res /= curr_res) r0) ++ [(curr_res, curr_robs)]

get_elem i p = do
    let (r0, r1, r2, r3) = p
    if i == 0 then r0 else do
       if i == 1 then r1 else do
          if i == 2 then r2 else r3

find_key1 k m = case Map.lookup k m of
    Nothing -> ([0, 0, 0, 0], -1)
    Just s -> s

find_key2 k m = case Map.lookup k m of
    Nothing -> ((0, 0, 0, 0), -1)
    Just s -> s

pass state max_robots max_time curr_max id hmap1 hmap2 = do
    let (res, rob, step) = state
    let curr_res = get_elem id res
    let curr_rob = rob !! id
    let time_remain = max_time - step
    let (cached_rob, c_step1) = find_key1 res hmap1
    let (cached_res, c_step2) = find_key2 rob hmap2

    if (c_step1 <= step && (all_gt_v cached_rob rob)) || (c_step2 <= step && (all_gt cached_res res)) then True else do
        curr_res + ((time_remain * (2 * curr_rob + time_remain)) `div` 2) < curr_max

dfs_m cost curr_res curr_rob curr_step max max_steps id max_robots cnt hmap1 hmap2 = do
    if curr_step == max_steps then do
        let curr_max = get_elem id curr_res
        let m_res = if max < curr_max then curr_max else max
        (m_res, cnt, hmap1, hmap2)
    else do
        -- let new_states = get_robots_resources_helper 0 curr_res (curr_res, curr_rob) 3 cost max_robots []
        let new_states = get_next_states curr_res curr_rob cost max_robots
        let next_states = map(\(nr, nb) -> ((add nr curr_rob), nb, curr_step + 1)) new_states
        let next_runable_states = filter(\s -> (pass s max_robots max_steps max id hmap1 hmap2) == False) next_states
        let new_cnt = cnt + (length next_runable_states)
        let (m_r, m_c, m1, m2) = foldl(\(m, c, m1, m2) (pr, pb, step) -> dfs_m cost pr pb step m max_steps id max_robots c m1 m2) (max, new_cnt, hmap1, hmap2) next_runable_states
        let new_m1 = foldl(\m (pr, pb, step) -> Map.insert pr (pb, step) m) m1 next_runable_states
        let new_m2 = foldl(\m (pr, pb, step) -> Map.insert pb (pr, step) m) m2 next_runable_states
        (m_r, m_c, new_m1, new_m2)


main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day19.txt")
    let r0 = map(\line -> Text.unpack line) input_list
    let costs = map(\line -> parse_line line) r0
    part1 costs
    part2 costs

part1 costs = do
    {-
        Couldn’t get my code to finish 24 steps initially is because I read the problem wrong :fast-panic:.
        You only have one robot factory so you can’t build arbitrary number of robots as long as resource is available, but only ONE robot a time!!!
        It now runs with bunch of pruning heuristics
        - never build more robots than highest resource robot building requirement for the resource the robot is collecting
        - get rid of the robot build configurations if with the same resource, we can strictly build more robots already in earlier steps (hashmap + DFS)
        - get rid of the robot build configurations if with the same robots, we have strictly more resources available in earlier steps (hashmap + DFS)
        - get rid of the robot build configurations if we know that we won’t be able to build more geode robots given the amount of time left (one robot a time is still the key here)
        - Still not super fast, but it gets the correct answer now:
        
        🍔 /usr/bin/time ./day19
        978
        15939
               53.98 real        52.77 user         0.76 sys
    -}



    let c0 = costs !! 0
    let max_robots = max_res_rob c0

    --let (res, robs, max_steps, curr_max, curr_step)  = ((2, 4, 0, 0), [1, 3, 1, 0], 25, 0, 11)
    --let (res, robs, max_steps, curr_max, curr_step)  = ((2, 4, 0, 0), [1, 3, 1, 0], 25, 0, 11)
    --let (res, robs, max_steps, curr_step)  = ((4, 33, 4, 5), [1, 4, 2, 2], 24, 22)
    --let (res, robs, max_steps, curr_step)  = ((3, 29, 2, 3), [1, 4, 2, 2], 24, 21)
    --let (res, robs, max_steps, curr_step)  = ((3, 13, 8, 0), [1, 4, 2, 0], 24, 17)
    --let (res, robs, max_steps, curr_step)  = ((3, 12, 0, 0), [1, 3, 0, 0], 24, 9)
    --let (res, robs, max_steps, curr_step)  = ((2, 9, 0, 0), [1, 3, 0, 0], 24, 8)
    --let (res, robs, max_steps, curr_step)  = ((2, 4, 0, 0), [1, 2, 0, 0], 24, 6)
    let (res, robs, max_steps, curr_step)  = ((1, 0, 0, 0), [1, 0, 0, 0], 24, 1)
    let curr_max = get_elem 3 res
    let (m, cnt, s1, s2) = dfs_m c0 res robs curr_step curr_max max_steps 3 max_robots 0 (Map.fromList []) (Map.fromList [])
    let r0 = map(\c -> dfs_m c res robs curr_step curr_max max_steps 3 max_robots 0 (Map.fromList []) (Map.fromList []) ) costs
    let r1 = zip (map(\p -> do
                                let (c, _, _, _) = p
                                c) r0) [1..]
    let result = sum (map(\p -> (fst p) * (snd p)) r1)
    print result


part2 costs = do
    let c0 = costs !! 0
    let max_robots = max_res_rob c0
    print max_robots
    let (res, robs, max_steps, curr_step)  = ((1, 0, 0, 0), [1, 0, 0, 0], 32, 1)
    let curr_max = get_elem 3 res

    let r0 = map(\c -> dfs_m c res robs curr_step curr_max max_steps 3 max_robots 0 (Map.fromList []) (Map.fromList []) ) (take 3 costs)
    let r1 = map(\p -> do
                                let (c, _, _, _) = p
                                c) r0
    let result = product r1
    print result






