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

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day9.txt")
    let r0 = map(\line -> Split.splitOn " " (Text.unpack line)) input_list
    let input = map(\line -> ((line !! 0), (read (line !! 1) :: Int))) r0
    part1 input
    part2 input

one_step :: (Int, Int)->String->(Int, Int)
one_step p dir
   | dir == "R" = ((fst p) + 1, (snd p))
   | dir == "L" = ((fst p) - 1, (snd p))
   | dir == "U" = ((fst p), (snd p) + 1)
   | dir == "D" = ((fst p), (snd p) - 1)

tail_step :: (Int, Int)->(Int, Int)->(Int, Int)
tail_step head_p tail_p = do
    let (h_x, h_y) = head_p
    let (t_x, t_y) = tail_p
    let dx = abs (h_x - t_x)
    let dy = abs (h_y - t_y)
    if dx <= 1 && dy <= 1 then (t_x, t_y) else
       if dx == 0 then do
         if h_y < t_y then (t_x, t_y - 1)
         else (t_x, t_y + 1)
       else do
         if dy == 0 then do
            if h_x < t_x then (t_x - 1, t_y)
            else (t_x + 1, t_y)
         else do
         -- dx != 1 && dy != 1 move diagonally
            if h_x < t_x && h_y < t_y then (t_x - 1, t_y - 1)
            else do
               if h_x > t_x && h_y < t_y then (t_x + 1, t_y - 1)
               else do
                  if h_x > t_x && h_y > t_y then (t_x + 1, t_y + 1)
                  else (t_x - 1, t_y + 1)

apply_tail_step i length rope prev_p = do
    if i == length then rope else do
        let curr_p = (rope !! i)
        let next_p = tail_step prev_p curr_p
        let new_rope = (take i rope) ++ [next_p] ++ (drop (i+1) rope)
        apply_tail_step (i + 1) length new_rope next_p

loop_move_n :: String -> Int -> Int -> [(Int, Int)]->Map(Int, Int) Int->([(Int, Int)], Map(Int, Int) Int)
loop_move_n dir steps i rope dict = do
 if i == steps then (rope, dict)
 else do
     let head_p = head rope
     let next_hp = one_step head_p dir
     let rope_tail = tail rope
     let next_rope_tail = apply_tail_step 0 (length rope_tail) rope_tail next_hp
     let next_rope = [next_hp] ++ next_rope_tail
     loop_move_n dir steps (i + 1) next_rope (Map.insert (last next_rope) 1 dict)

move_n :: ([(Int, Int)], Map (Int, Int) Int) -> (String, Int)->([(Int, Int)], Map(Int, Int) Int)
move_n rope_dict action = do
    let (rope, dict) = rope_dict
    let (dir, steps) = action
    loop_move_n dir steps 0 rope dict

part1 input = do
    let dict :: Map (Int, Int) Int = Map.fromList[]
    let rope = take 2 (repeat (0, 0))
    let (rope_f, dict_f) = foldl(\rope_dict action -> move_n rope_dict action) (rope, dict) input
    print (Map.size dict_f)

part2 input = do
    let dict :: Map (Int, Int) Int = Map.fromList[]
    let rope = take 10 (repeat (0, 0))
    let (rope_f, dict_f) = foldl(\rope_dict action -> move_n rope_dict action) (rope, dict) input
    print (Map.size dict_f)
