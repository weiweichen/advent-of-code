import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

get_starting_item input = do
    let r = drop (length "  Starting items: ") input
    map(\item -> read item :: Integer)(Split.splitOn ", " r)

get_true_false_monkeys t_input f_input = do
    let t_m = read (drop (length "    If true: throw to monkey ") t_input) :: Int
    let f_m = read (drop (length "    If false: throw to monkey ") f_input) :: Int
    (t_m, f_m)

get_div_by input = do
    read (drop (length "  Test: divisible by ") input) :: Integer

one_monkey id i num_items starting_items true_false_monkeys div_bys op p manage = do
    if i == num_items then starting_items else do
       let (t_m, f_m) = true_false_monkeys
       let item = head (starting_items !! id)
       let worry_level = op item
       let bored_value = if manage then (worry_level `div` 3) else worry_level
       let div_by = div_bys !! id
       let tf_value = (bored_value `mod` div_by) == 0
       let throw_to = if tf_value then t_m else f_m
       let next_item = if manage then bored_value else bored_value `mod` p

       let r0 = (take id starting_items) ++ [tail (starting_items !! id)] ++ (drop (id + 1) starting_items)
       let new_si = (take throw_to r0 ) ++ [(r0 !! throw_to) ++ [next_item]] ++ (drop (throw_to + 1) r0)
       one_monkey id (i + 1) num_items new_si true_false_monkeys div_bys op p manage

one_round m total_monkeys starting_items true_false_monkeys div_bys ops inspections p manage = do
    if m == total_monkeys then (starting_items, inspections) else do
        let num_items = length (starting_items !! m)
        let tf_monkeys = true_false_monkeys !! m
        let op = ops !! m
        let new_si = one_monkey m 0 num_items starting_items tf_monkeys div_bys op p manage
        let new_ins = (take m inspections) ++ [(inspections !! m) + num_items] ++ (drop (m + 1) inspections)
        one_round (m + 1) total_monkeys new_si true_false_monkeys div_bys ops new_ins p manage

run_rounds i total_rounds total_monkeys starting_items true_false_monkeys div_bys ops inspections p manage = do
    if i == total_rounds then (starting_items, inspections) else do
       let (new_si, new_inspections) = one_round 0 total_monkeys starting_items true_false_monkeys div_bys ops inspections p manage
       run_rounds (i + 1) total_rounds total_monkeys new_si true_false_monkeys div_bys ops new_inspections p manage

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day11.txt")
    let r0 = map(\info -> (map (\t -> Text.unpack t) info))(Split.splitOn (Text.pack(""):[]) input_list)
    let num_monkeys = length r0
    let starting_items = map(\info -> get_starting_item (info !! 1)) r0
    let true_false_monkeys = map(\info -> get_true_false_monkeys (info !! 4) (info !! 5)) r0
    let div_bys = map(\info -> get_div_by (info !! 3)) r0
    -- let ops = [(\old -> old * 19), (\old -> old + 6), (\old-> old * old), (\old -> old + 3)]
    let ops = [(\old -> old * 5), (\old -> old + 3), (\old-> old + 7), (\old -> old + 5), (\old -> old + 2), (\old-> old * 19), (\old -> old * old), (\old -> old + 4)]
    let num_inspections = take num_monkeys (repeat 0)

    part1 num_monkeys starting_items true_false_monkeys div_bys ops num_inspections
    part2 num_monkeys starting_items true_false_monkeys div_bys ops num_inspections


part1 num_monkeys starting_items true_false_monkeys div_bys ops num_inspections = do
    let (items, actions )= run_rounds 0 20 num_monkeys starting_items true_false_monkeys div_bys ops num_inspections (product div_bys) True
    let result = product (take 2 (reverse (sort actions)))
    print result

part2 num_monkeys starting_items true_false_monkeys div_bys ops num_inspections = do
    let (items, actions) = run_rounds 0 10000 num_monkeys starting_items true_false_monkeys div_bys ops num_inspections (product div_bys)  False
    let result = product (take 2 (reverse (sort actions)))
    print result

