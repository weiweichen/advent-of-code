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
    let name = r0 !! 0
    let op_or_number = Split.splitOn " " (r0 !! 1)
    let number = if (length op_or_number) == 1 then (read (op_or_number !! 0) :: Integer) else 0
    let neighbours = if (length op_or_number) == 3 then ((op_or_number !! 0), (op_or_number !! 2))  else ("", "")
    let op = if (length op_or_number) == 3 then (op_or_number !! 1) else ""
    (name, (number, neighbours, op))

perform_op op v1 v2
    | op == "+" = v1 + v2
    | op == "*" = v1 * v2
    | op == "-" = v1 - v2
    | op == "/" = v1 `div` v2
    | otherwise = -1

at_part1 key m = case Map.lookup key m of
    Nothing -> (-1, ("", ""), "")
    Just s -> s

at_part2 key m = case Map.lookup key m of
    Nothing -> (False, "ahhhh")
    Just s -> s


yell_what monkey map = do
    let (number, (n1, n2), op) = at_part1 monkey map
    if op == "" then (number, map) else do
        let (v1, m1) = yell_what n1 map
        let (v2, m2) = yell_what n2 m1
        let res = perform_op op v1 v2
        let new_map = Map.insert monkey (res, (n1, n2), "") m2
        (res, new_map)

contains_me me monkey map1 map2 = do
    let (number, (n1, n2), op) = at_part1 monkey map1
    if op == "" then (monkey == me, map2) else do
        let (v1, m1) = contains_me me n1 map1 map2
        let (v2, m2) = contains_me me n2 map1 m1
        let res = v1 || v2
        let new_map = Map.insert monkey (res, "") m2
        (res, new_map)

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day21.txt")
    let r0 = map(\line -> parse_line (Text.unpack line)) input_list
    part1 r0
    part2 r0

find_my_value lower upper target map me root = do
    let i = lower + (upper - lower) `div` 2
    if lower >= upper - 1 then -1 else do
        let new_map = Map.insert me (i, ("", ""), "") map
        let (v, _) = yell_what root new_map
        if v == target then i else do
            if v > target then find_my_value i upper target map me root
            else find_my_value lower i target map me root

part1 list = do
    let r0 :: Map String (Integer, (String, String), String) = Map.fromList list
    let (res, m) = yell_what "root" r0
    print res

part2 list = do
    let me = "humn"
    let r0 :: Map String (Integer, (String, String), String) = Map.fromList list
    let (_, (root_n1, root_n2), _) = at_part1 "root" r0

    let r1 = map(\line -> do
                            let (name, (number, (n1, n2), op)) = line
                            (name, (name == me, op))) list
    let r2 = Map.fromList r1
    let (_, r3) = contains_me me "root" r0 r2
    let left = at_part2 root_n1 r3
    let right = at_part2 root_n2 r3
    let (left_v, _) = yell_what root_n1 r0
    let (right_v, _) = yell_what root_n2 r0

    -- aoc seems to only accept the smallest number that satisfy this, however, multiple number would work here
    -- including 3582317956029 3582317956030 3582317956031 3582317956032 ... probably because integer div
    let res = find_my_value 0 3582317956030 right_v r0 me root_n1
    print res

