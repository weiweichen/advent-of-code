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

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day18.txt")
    let r0 = map(\line -> Text.unpack line) input_list
    let r1 = map(\line -> Split.splitOn "," line) r0
    let r2 = map(\line -> map(\p -> read p :: Int) line) r1
    let r3 = map(\line -> ((line !! 0), (line !! 1), (line !! 2))) r2
    part1 r3
    part2 r3

calc_faces point p_set = do
    let dirs = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0 , 1)]
    let (x, y, z) = point
    let neighbors = map(\(dx, dy, dz) -> (x + dx, y + dy, z + dz)) dirs
    foldl(\r n -> if (Set.member n p_set) then r else r + 1) 0 neighbors

neighbors :: (Num a) => (a, a, a) -> [(a, a, a)]
neighbors (x, y, z) =
    [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

flood max_r min_r pset seen [] = seen
flood max_r min_r pset seen (point:q) = flood max_r min_r pset (Set.union seen (Set.fromList next)) (q ++ next) where
    next = filter ok $ neighbors point
    ok point@(x, y, z) =
        inRange (min_r - 1, max_r + 1) x &&
        inRange (min_r - 1, max_r + 1) y &&
        inRange (min_r - 1, max_r + 1) z &&
        Set.notMember point pset &&
        Set.notMember point seen

calc_faces_part2 point p_set seen_set = do
    let dirs = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0 , 1)]
    let (x, y, z) = point
    let neighbors = map(\(dx, dy, dz) -> (x + dx, y + dy, z + dz)) dirs
    foldl(\r n -> if ((Set.member n p_set) || (Set.member n seen_set /= True)) then r else r + 1) 0 neighbors

part1 r3 = do
    let point_set :: Set (Int, Int, Int) = Set.fromList r3
    let result = foldl(\r p -> (calc_faces p point_set) + r) 0 r3
    print result

part2 r3 = do
    let point_set :: Set (Int, Int, Int) = Set.fromList r3
    let xs = (map(\(x, y, z) -> x) r3)
    let ys = (map(\(x, y, z) -> y) r3)
    let zs = (map(\(x, y, z) -> z) r3)
    let (max_x, min_x) = (maximum xs, minimum xs)
    let (max_y, min_y) = (maximum ys, minimum ys)
    let (max_z, min_z) = (maximum zs, minimum zs)
    let min_r = minimum [min_x, min_y, min_z]
    let max_r = maximum [max_x, max_y, max_z]

    let init_s :: Set (Int, Int, Int) = Set.fromList []
    let result_set = flood max_r min_r point_set init_s [(min_r, min_r, min_r)]

    let result = foldl(\r p -> (calc_faces_part2 p point_set result_set) + r) 0 r3
    print result




