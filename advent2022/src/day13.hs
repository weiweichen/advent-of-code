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

find_close_parenthesis i len curr_open input = do
    let c = input !! i
    if curr_open == 1 && c == ']' then i else do
        if c == '[' then find_close_parenthesis (i + 1) len (curr_open + 1) input else do
            if c == ']' then find_close_parenthesis (i + 1) len (curr_open - 1) input else do
                find_close_parenthesis (i + 1) len curr_open input

split_one_level :: Int->Int->String->[String]->[String]
split_one_level i len input result = do
    if i == len then result else do
        let c = input !! i
        if c /= '[' && c /= ',' then do
            split_one_level (i + 1) len input ((init result) ++ [(last result) ++ [c]])
        else do
            if c == ',' then do
               split_one_level (i + 1) len input (result ++ [""])
            else do
               let close = find_close_parenthesis (i + 1) len 1 input
               let curr_slice = drop i (take (close + 1) input)
               let new_result = (init result) ++ [(last result) ++ curr_slice]
               split_one_level (close + 1) len input new_result

to_list item = do
    if (length item) == 0 then [item] else do
        if item !! 0 == '[' then do
           let input = init (tail item)
           split_one_level 0 (length input) input [""]
        else [item]

compare_lists i l_list r_list len_l len_r = do
    if i < len_l && i < len_r then do
        let l_item = l_list !! i
        let r_item = r_list !! i
        let comp = compare_item l_item r_item
        if comp /= EQ then comp else do
            compare_lists (i + 1) l_list r_list len_l len_r
    else do
        if i == len_l && i <= len_r then LT
        else GT

compare_item left right = do
    let left_items = to_list left
    let right_items = to_list right
    let length_l = length left_items
    let length_r = length right_items
    if length_l == 0 && length_r > 0 then  LT else do
        if length_l > 0 && length_r == 0 then  GT else do
            if length_l == 0 && length_r == 0 then  LT else do
                if length_l == 1 && length_r == 1 then do
                    let l_item = head left_items
                    let r_item = head right_items
                    let ll = length l_item
                    let lr = length r_item
                    if ll == 0 && lr == 0 then EQ else do
                        if ll == 0 && lr /= 0 then LT else do
                            if ll /= 0 && lr == 0 then GT else do
                                if (head l_item) /= '[' && (head r_item) /= '[' then do
                                    let l_value = read (head left_items) :: Integer
                                    let r_value = read (head right_items) :: Integer
                                    if l_value == r_value then EQ else do
                                        if l_value < r_value then LT else GT
                                else compare_item (head left_items) (head right_items)

                else compare_lists 0 left_items right_items length_l length_r

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day13.txt")
    let input_pairs = map(\info -> (map (\t -> Text.unpack t) info))(Split.splitOn (Text.pack(""):[]) input_list)
    part1 input_pairs
    part2 input_pairs


run i n input_pairs = do
    if i < n then do
        let pair = input_pairs !! i
        let left = pair !! 0
        let right = pair !! 1
        let c = compare_item left right
        print pair
        print c
        run (i + 1) n input_pairs
    else do
        print "end"


f_helper found_index target item = do
    let found = fst found_index
    let index = snd found_index
    if found then found_index else do
        if target == item then (True, index + 1)
        else (False, index + 1)


part1 input_pairs = do
    let r0 = map(\pair -> compare_item (pair !! 0) (pair !! 1)) input_pairs
    let r1 = filter(\pair -> (fst pair) == LT) (zip r0 [1..])
    let indices = map(\pair -> snd pair) r1
    print (sum indices)

part2 input_pairs = do
    let r0 = input_pairs >>= \pair -> [pair !!0, pair !! 1]
    let r1 = r0 ++ ["[[2]]"] ++ ["[[6]]"]
    let r2 = sortBy compare_item r1

    let i2 = foldl(\fi x -> f_helper fi "[[2]]" x) (False, 0) r2
    let i6 = foldl(\fi x -> f_helper fi "[[6]]" x) (False, 0) r2
    print ((snd i2) * (snd i6))

-- extremely pretty implementation on reddit as below:
-- https://www.reddit.com/r/adventofcode/comments/zkmyh4/comment/j01t040/?utm_source=share&utm_medium=web2x&context=3


data IntOrList = I Int | L [IntOrList] deriving (Show, Eq, Read)

instance Ord IntOrList where
    compare (I i1) (I i2) = compare i1 i2
    compare (L xs) (L ys) = compare xs ys
    compare (I x) (L ys) = compare (L [I x]) (L ys)
    compare (L xs) (I y) = compare (L xs) (L [I y])

readIOL :: String -> IntOrList
readIOL "" = L []
readIOL pstr = L [read $ stringPreprocessor pstr]
    where
        stringPreprocessor "" = ""
        stringPreprocessor str@(c:cs)
            | c == '[' = "L [" ++ stringPreprocessor cs
            | c == ' ' = ' ' : stringPreprocessor cs
            | c == ',' = ',' : stringPreprocessor cs
            | c == ']' = ']' : stringPreprocessor cs
            | otherwise = "I " ++ (takeWhile isNumeric str) ++ (stringPreprocessor (dropWhile isNumeric str))
        isNumeric = (flip elem) "-0123456789"

q1 :: IO Int
q1 = countRightOrders 1 0 <$> puzzleInput

q2 :: IO Int
q2 = (dividerIndicesProduct (dividers []) 1).sort.dividers <$> puzzleInput

pretty_main :: IO ()
pretty_main = q1 >>= print >> q2 >>= print

puzzleInput :: IO [IntOrList]
puzzleInput = (filter (/= (L []))).(fmap readIOL).lines <$> readFile "input.txt"

dividers :: [IntOrList] -> [IntOrList]
dividers = ((readIOL "[[2]]"):).((readIOL "[[6]]"):)

dividerIndicesProduct :: [IntOrList] -> Int -> [IntOrList] -> Int
dividerIndicesProduct [] _ _ = 1
dividerIndicesProduct _ _ [] = error "Not all dividers found"
dividerIndicesProduct (d:ds) n (p:ps)
    | p == d = n * (dividerIndicesProduct ds (n+1) ps)
    | otherwise = (dividerIndicesProduct (d:ds) (n+1) ps)

countRightOrders :: Int -> Int -> [IntOrList] -> Int
countRightOrders n acc [] = acc
countRightOrders n acc (x:y:zs)
    | compare x y == GT = countRightOrders (n+1) acc zs
    | otherwise = countRightOrders (n+1) (acc+n) zs

