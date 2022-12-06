import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Typeable

main = do
  part1
  part2

move_from_to :: ([Char], Int) -> Int -> Int -> Int -> [Char]-> [Char]
move_from_to pair n from to add = do
    let s = (fst pair)
    let idx = (snd pair)
    if idx == from then (drop n s) else do
      if idx == to then (add ++ s) else s

action :: [[Char]]->(Int, Int, Int)-> Bool->[[Char]]
action s m is_reverse = do
    let (cnt, from, to) = m
    let from_s = fst (filter(\pair -> (snd pair) == from) (zip s [1..]) !! 0)
    let add = if is_reverse then (reverse (take cnt from_s)) else (take cnt from_s)
    map(\pair -> (move_from_to pair cnt from to add)) (zip s [1..])

part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day5.txt")
    let r0 = (Split.splitOn (Text.pack(""):[]) input_list)
    let s0 = map(\s -> (Text.unpack s)) (take ((length (r0 !! 0)) - 1) (r0 !! 0))
    let s1 = map(\s -> filter(\p -> ((snd p) - 2) `mod` 4 == 0)(zip (Text.unpack s)[1..])) (take ((length (r0 !! 0)) - 1) (r0 !! 0))
    let s2 = transpose ((map(\line -> map(\p -> (fst p)) line) s1))
    let s = map(\line -> filter(\x -> x /= ' ') line) s2
    let moves = map(\m -> ((read (m!!1) :: Int), read(m!!3)::Int,  read (m!!5) :: Int)) (map(\line -> Split.splitOn " " (Text.unpack line) )(r0 !! 1))
    let r = foldl(\r m -> action r m True) s moves
    let result = map(\s -> (s!!0)) r
    print result

part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day5.txt")
    let r0 = (Split.splitOn (Text.pack(""):[]) input_list)
    let s0 = map(\s -> (Text.unpack s)) (take ((length (r0 !! 0)) - 1) (r0 !! 0))
    let s1 = map(\s -> filter(\p -> ((snd p) - 2) `mod` 4 == 0)(zip (Text.unpack s)[1..])) (take ((length (r0 !! 0)) - 1) (r0 !! 0))
    let s2 = transpose ((map(\line -> map(\p -> (fst p)) line) s1))
    let s = map(\line -> filter(\x -> x /= ' ') line) s2
    let moves = map(\m -> ((read (m!!1) :: Int), read(m!!3)::Int,  read (m!!5) :: Int)) (map(\line -> Split.splitOn " " (Text.unpack line) )(r0 !! 1))
    let r = foldl(\r m -> action r m False) s moves
    let result = map(\s -> (s!!0)) r
    print result


