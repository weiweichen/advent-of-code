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


parse_line line = do
    let r0 = Split.splitOn "; " line
    let part1 = Split.splitOn " " (r0 !! 0)
    let part2 = Split.splitOn ", " (r0 !! 1)
    let name = (part1 !! 1)
    let rate = read (drop (length "rate=") (last part1)) :: Int
    let prefix = if (length part2 == 1) then "tunnel leads to valve " else "tunnel leads to valves "
    let neighbours = [drop (length prefix)(head part2)] ++ (tail part2)
    (name, (rate, neighbours))


-- ([String], value, mins)

get_node_id name nmap = case Map.lookup name nmap of
                 Nothing  -> (-1, (-1, []))
                 Just s -> s


init_node node nmap t_nodes = do
    let (name, (idx, (rate, neighbour))) = node
    let r0 = take t_nodes (repeat (t_nodes * 2))
    -- let r0 = take t_nodes (repeat (88))
    let r1 = (take idx r0) ++ [0] ++ (drop (idx + 1) r0)

    foldl(\r n -> do let (idx, (_, _)) = get_node_id n nmap
                     take idx r ++ [1] ++ (drop (idx + 1) r)) r1 neighbour

init_sg nodes nmap t_nodes =  do
    foldl(\r node -> r ++ [init_node node nmap t_nodes]) [] nodes


update_tbl i j v tbl = do
    let line_i = (tbl !! i)
    let new_line = (take j line_i) ++ [v] ++ (drop (j + 1) line_i)
    take i tbl ++ [new_line] ++ (drop (i + 1) tbl)


shortest_path i j k t_nodes fw_tbl = do
    if k == t_nodes then fw_tbl else do
        if j == t_nodes then shortest_path 0 0 (k + 1) t_nodes fw_tbl else do
            if i == t_nodes then shortest_path 0 (j + 1) k t_nodes fw_tbl else do
                let ij = (fw_tbl !! i) !! j
                let ik = (fw_tbl !! i) !! k
                let kj = (fw_tbl !! k) !! j
                let new_tbl = if ij > ik + kj then update_tbl i j (ik + kj) fw_tbl else fw_tbl
                shortest_path (i + 1) j k t_nodes new_tbl

find_value opened result = case Map.lookup opened result of
    Nothing -> 0
    Just s -> s

find_b name bmap = case Map.lookup name bmap of
    Nothing -> -1
    Just s -> s

visit :: Int -> Integer->Int->Int->Int->[(String, (Int, (Int, [String])))]->[[Int]]->Map Integer Int-> Map String Int ->Map Integer Int
visit idx opened time_left t_nodes value list tbl result bmap = do
    if time_left <= 0 then result else do
        let new_result = Map.insert opened (maximum([(find_value opened result), value])) result
        let row = (tbl !! idx)
        foldl(\r (dis, id) -> do
                let (n, (i, (v, _))) = list !! id
                let b = find_b n bmap
                let already_visited = if b == -1 then True else (opened .&. (1 `shift` b))  /= 0
                if dis == 0 || dis >= t_nodes || already_visited then r else do
                    let new_state = (opened .|. (1 `shift` b))
                    let new_time_left = time_left - dis - 1;
                    visit i new_state new_time_left t_nodes (value + (new_time_left * v)) list tbl r bmap
            ) new_result (zip row [0..])

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day16.txt")
    part1 input_list
    part2 input_list


part1 input_list = do
    let r0 = map(\line -> (Text.unpack line)) input_list
    let r1 = map(\line -> parse_line line) r0
    let r2 = map(\line -> (fst(fst line), ((snd line), snd(fst line)))) (zip r1 [0..])
    let nmap = Map.fromList r2

    let init_tbl = init_sg r2 nmap (length r2)
    let fw_tbl = shortest_path 0 0 0 (length r2) init_tbl
    let r3 = filter(\line -> fst(snd line) /= 0) r1
    let node_bit_map :: Map String Int = Map.fromList(map(\line -> (fst (fst line), (snd line))) (zip r3 [0..]))
    let rmap :: Map Integer Int = Map.fromList []
    let aa_idx = fst (get_node_id "AA" nmap)
    let result = visit aa_idx 0 30 (length r2) 0 r2 fw_tbl rmap node_bit_map
    print (maximum (map(\pair -> snd pair) (Map.toList result)))

part2 input_list = do
    let r0 = map(\line -> (Text.unpack line)) input_list
    let r1 = map(\line -> parse_line line) r0
    let r2 = map(\line -> (fst(fst line), ((snd line), snd(fst line)))) (zip r1 [0..])
    let nmap = Map.fromList r2

    let init_tbl = init_sg r2 nmap (length r2)
    let fw_tbl = shortest_path 0 0 0 (length r2) init_tbl
    let r3 = filter(\line -> fst(snd line) /= 0) r1
    let node_bit_map :: Map String Int = Map.fromList(map(\line -> (fst (fst line), (snd line))) (zip r3 [0..]))
    let rmap :: Map Integer Int = Map.fromList []
    let aa_idx = fst (get_node_id "AA" nmap)
    let res_map = visit aa_idx 0 26 (length r2) 0 r2 fw_tbl rmap node_bit_map
    let mask :: Integer = (1 `shift` ((length r3))) - 1

    -- why is this soooooo fast????
    let release = [v1 + v2 | (s1, v1) <- Map.toList res_map
                             , (s2, v2) <- Map.toList res_map
                             , (s1 .&. s2) == 0]

    print (maximum release)



