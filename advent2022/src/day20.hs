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

find_id id j [] = (0, j)
find_id id j (x:xs) = (v, k) where
    (value, i) = x
    (v, k) = if i == id then (value, j) else find_id id (j + 1) xs

find_v v j [] = -1
find_v v j (x:xs) = k where
    k = if v == (fst x) then j else find_v v (j + 1) xs

reorder nth list = do
    let (shift_value, curr_pos) = find_id nth 0 list
    let l = length list
    if shift_value == 0 then list else do
        let before = take curr_pos list
        let after = drop (curr_pos + 1) list
        let new_list = after ++ before
        let pos = fromInteger((abs shift_value) `mod` ((toInteger l) - 1))

        if shift_value < 0 then do
            let new_before = take (l - 1 - pos) new_list
            let new_after = drop (l - 1 - pos) new_list
            new_before ++ [(shift_value, nth)] ++ new_after
        else do
            let new_before = take pos new_list
            let new_after = drop pos new_list
            new_before ++ [(shift_value, nth)] ++ new_after

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day20.txt")
    let r0 = map(\line -> read (Text.unpack line) :: Integer) input_list
    part1 r0
    part2 r0

part1 r0 = do
    let r1 = zip r0 [0..]
    let r2 = foldl(\l i -> reorder i l) r1 (take (length r1) [0..])
    let pos_zero = find_v 0 0 r2
    let r3 = (drop pos_zero r2) ++ (take pos_zero r2)
    let l = length r3
    let r4= map(\n -> r3 !! (n `mod` l)) [1000, 2000, 3000]
    let result = sum (map (\p -> fst p) r4)
    print result

one_round list = do
    foldl(\l i -> reorder i l) list (take (length list) [0..])

part2 r0 = do
    let r1 = zip (map(\v -> v * 811589153) r0) [0..]
    let r2 = foldl(\l i -> one_round l) r1 (take 10 [0..])
    let pos_zero = find_v 0 0 r2
    let r3 = (drop pos_zero r2) ++ (take pos_zero r2)
    let l = length r3
    let r4= map(\n -> r3 !! (n `mod` l)) [1000, 2000, 3000]
    let result = sum (map (\p -> fst p) r4)
    print result





