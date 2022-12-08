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

uniquify lst = toList $ fromList lst

main = do
    part1
    part2

mygroup :: Int->Int->[[[Char]]]->[[[[Char]]]]->[[[[Char]]]]
mygroup size i input prev_result = do
   if i == size then prev_result else do
      let curr_input = (input !! i)
      if (curr_input !! 1) == "cd" then do
         mygroup size (i + 1) input (prev_result ++ [[curr_input]])
      else do
         mygroup size (i + 1) input ((init prev_result) ++ [(last prev_result) ++ [curr_input]])

group_stats :: [[[Char]]] -> ([Char], (Integer, [[Char]]))
group_stats g = do
  let group_name = (g !! 0) !! 2
  let files = filter(\item -> (item !! 0) /= "dir" && (item !!0) /= "$") g
  let files_sizes = map(\item -> read (item !! 0) :: Integer) files
  let dirs = filter(\item -> (item !! 0) == "dir") g
  let dir_names = map(\item -> (item !! 1)) dirs
  (group_name, ((sum files_sizes), dir_names))

find_size :: String -> Map String Integer -> Integer
find_size name map = case Map.lookup name map of
                 Nothing  -> -1
                 Just s -> s

find_stats :: String -> Map String (Integer, [String]) -> (Integer, [String])
find_stats name map = case Map.lookup name map of
    Nothing -> (-1, [])
    Just s -> s


get_edge :: (String, (Integer, [String])) -> (String, String, [String])
get_edge stat = do
  let (name, (_, kids)) = stat
  (name, name, kids)


calc_kids_sizes :: [String] -> Map String Integer -> Map String (Integer, [String]) -> Map String Integer
calc_kids_sizes kids nodeSizeMap nodeStatsMap = do
    foldl(\ns kid -> dfs kid ns nodeStatsMap) nodeSizeMap kids

dfs :: String -> Map String Integer -> Map String (Integer, [String]) -> Map String Integer
dfs node nodeSizeMap nodeStatsMap = do
    let (file_size, kids) = find_stats node nodeStatsMap
    let nodeSize = find_size node nodeSizeMap
    if nodeSize /= -1 then nodeSizeMap else do
       if (length kids) == 0 then Map.insert node file_size nodeSizeMap
       else do
         let ns = calc_kids_sizes kids nodeSizeMap nodeStatsMap
         let kids_sizes = map(\kid -> find_size kid ns) kids
         Map.insert node (file_size + (sum kids_sizes)) ns

full_name :: ([String], [([String], (Integer, [String]))]) -> (String, (Integer, [String])) -> ([String], [([String], (Integer, [String]))])
full_name name_result stat = do
    let (prev_names, prev_result) = name_result
    let (curr_name, stats) = stat
    let curr_names = if curr_name == ".." then init prev_names else prev_names ++ [curr_name]
    let curr_dir_names = if curr_name == ".." then [".."] else curr_names
    (curr_names, prev_result ++ [(curr_dir_names, stats)])

update_kid_name :: (String, (Integer, [String])) -> (String, (Integer, [String]))
update_kid_name item = do
    let (name, (size, kids)) = item
    let new_kids = map(\kid -> name ++ kid) kids
    (name, (size, new_kids))

part1 input_list = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day7.txt")
    let r0 = map(\line -> Split.splitOn " " (Text.unpack line)) input_list
    let r1 = (mygroup (length r0) 0 r0 [])
    let r2 = map(\g -> (group_stats g)) r1
    let (_, r3) = foldl(\name_result stat -> full_name name_result stat ) ([], []) r2
    let r4 = map(\item -> ((concat (intersperse "" (fst item))), snd item)) r3
    let r5 = filter(\item -> (fst item) /= "..") r4
    let r6 = map(\item -> (update_kid_name item)) r5
    let stats_dict = Map.fromList r6
    let dir_sizes :: Map String Integer = Map.fromList[]
    let dfs_result = dfs "/" dir_sizes stats_dict
    let result_list = Map.toList dfs_result
    let result = map(\item -> snd item)(filter(\r -> (snd r) <= 100000) result_list)
    let full_size = find_size "/" dfs_result
    print (sum result)

part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day7.txt")
    let r0 = map(\line -> Split.splitOn " " (Text.unpack line)) input_list
    let r1 = (mygroup (length r0) 0 r0 [])
    let r2 = map(\g -> (group_stats g)) r1
    let (_, r3) = foldl(\name_result stat -> full_name name_result stat ) ([], []) r2
    let r4 = map(\item -> ((concat (intersperse "" (fst item))), snd item)) r3
    let r5 = filter(\item -> (fst item) /= "..") r4
    let r6 = map(\item -> (update_kid_name item)) r5
    let stats_dict = Map.fromList r6
    let dir_sizes :: Map String Integer = Map.fromList[]
    let dfs_result = dfs "/" dir_sizes stats_dict
    let result_list = Map.toList dfs_result
    let full_size = find_size "/" dfs_result
    let r7 = filter(\item -> ((snd item) + 70000000 - full_size) >= 30000000) result_list
    let result = map(\item -> snd item) r7
    print (minimum result)

