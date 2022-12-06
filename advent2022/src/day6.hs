import System.IO
import Data.List as List
import Data.List.Split as Split
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Typeable
import Data.Set (toList, fromList)
uniquify lst = toList $ fromList lst

main = do
  part1
  part2

check4 :: Int->[Char]->Int
check4 _ [] = 0
check4 i (a1:a2:a3:a4:rest)
  | length(uniquify (a1:a2:a3:a4:[])) == 4 = i + 3
  | otherwise = check4 (i+1) (a2:a3:a4:rest)

check14 :: Int->[Char]->Int
check14 _ [] = 0
check14 i (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:rest)
  | length(uniquify (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:[])) == 14 = i + 13
  | otherwise = check14 (i+1) (a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:rest)


part1 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day6.txt")
    let r0 = (map(\x -> Text.unpack x) input_list)!!0
    let result = (check4 1 r0)
    print result

part2 = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day6.txt")
    let r0 = (map(\x -> Text.unpack x) input_list)!!0
    let result = (check14 1 r0)
    print result
