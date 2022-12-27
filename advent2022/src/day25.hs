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
import Data.Array

snafu_to_number snafu
    | snafu == "=" = -2
    | snafu == "-" = -1
    | snafu == "0" = 0
    | snafu == "1" = 1
    | snafu == "2" = 2
    | otherwise = result where
        (pow, result) = foldl(\(p, r) s -> (p * 5, r + p * (snafu_to_number [s]))) (1, 0) (reverse snafu)

neg_val pairs = do
    map(\p -> (-(fst p), (snd p))) pairs

number_to_snafu_helper :: Integer->[(Integer, Integer)]
number_to_snafu_helper number
    | number == -2 = [(-2, 0)]
    | number == -1 = [(-1, 0)]
    | number == 0 = [(0, 0)]
    | number == 1 = [(1, 0)]
    | number == 2 = [(2, 0)]
    | otherwise = result where
        fp = fromInteger number :: Float
        p = floor (logBase 5 fp)
        l5 = (5 ^ p)
        u5 = (5 ^ (p + 1))
        p1 = div (u5 - 1)  2
        --result = [(u5, u5)]
        result
            | number > p1 = [(1, (p+1))] ++ (neg_val (number_to_snafu_helper (u5 - number)))
            | number >= 2 * l5 = [(2, p)] ++ (number_to_snafu_helper (number - (2 * l5)))
            | otherwise = [(1, p)] ++ (number_to_snafu_helper (number - l5))

value_to_symbol v
    | v == 2 = "2"
    | v == 1 = "1"
    | v == -1 = "-"
    | v == -2 = "="
    | v == 0 = "0"


number_to_snafu number = do
    let r0 = number_to_snafu_helper number
    let r1 = foldl(\q (curr_v, curr_p) -> do
                                           let (prev_v, prev_p) = last q
                                           if curr_p == prev_p then
                                               (init q) ++ [(curr_v + prev_v, curr_p)]
                                           else q ++ [(curr_v, curr_p)]
                                           ) [(head r0)] (tail r0)

    let (res, _) = foldl(\(r, p1) (v, p2) -> do
                                                let p2' :: Int = fromInteger p2
                                                if p1 > (p2' + 1) then do
                                                    ((r ++ (take (p1 - p2' - 1) (repeat '0'))) ++ (value_to_symbol v), p2')
                                                else (r ++ (value_to_symbol v), p2')
                                            ) ("", (length r1)) r1
    res

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day25.txt")
    let inputs = map(\line -> Text.unpack line) input_list
    part1 inputs

part1 inputs = do
    let r0 = map(\s -> snafu_to_number s) inputs
    let res = number_to_snafu (sum r0)
    print res
    let r = snafu_to_number "2=-0=01----22-0-1-10"
    print r





