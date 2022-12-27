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


parse valley = (elves, (n_wind, s_wind, w_wind, e_wind)) where
    (width, height) = (length (head valley), length valley)
    n_wind = array ((0, 0), (height - 1, width - 1)) (concat [[((iy, ix), bliz == '^') | (ix, bliz) <- zip [0..] row ] | (iy, row) <- zip [0..] valley])
    s_wind = array ((0, 0), (height - 1, width - 1)) (concat [[((iy, ix), bliz == 'v') | (ix, bliz) <- zip [0..] row ] | (iy, row) <- zip [0..] valley])
    w_wind = array ((0, 0), (height - 1, width - 1)) (concat [[((iy, ix), bliz == '<') | (ix, bliz) <- zip [0..] row ] | (iy, row) <- zip [0..] valley])
    e_wind = array ((0, 0), (height - 1, width - 1)) (concat [[((iy, ix), bliz == '>') | (ix, bliz) <- zip [0..] row ] | (iy, row) <- zip [0..] valley])
    elves = array ((0, 0), (height - 1, width - 1)) (concat [[((iy, ix), False) | (ix, bliz) <- zip [0..] row ] | (iy, row) <- zip [0..] valley])

is_blizzard x y turn blizzard_map = is_n || is_s || is_w || is_e where
    (n_wind, s_wind, w_wind, e_wind) = blizzard_map
    ((_, _), (height, width)) = bounds n_wind
    is_n = n_wind ! (((y + turn) `mod` (height + 1)), x)
    is_s = s_wind ! (((y - turn) `mod` (height + 1)), x)
    is_w = w_wind ! (y, ((x + turn) `mod` (width + 1)))
    is_e = e_wind ! (y, ((x - turn) `mod` (width + 1)))

next_emap bmap elves sx sy turn = array ((0, 0), (height, width)) (concat [ [((y, x), (is_elve x y)) | x <- [0..width]] | y <- [0..height]]) where
    ((_, _), (height, width)) = bounds elves
    is_elve x y
        | is_blizzard x y turn bmap          = False
        | x == sx && y == sy                 = True
        | elves ! (y, x)                     = True
        | x > 0 && elves ! (y, (x-1))        = True
        | x < width && elves ! (y, (x + 1))  = True
        | y > 0 && elves ! ((y - 1), x)      = True
        | y < height && elves ! ((y + 1), x) = True
        | otherwise                          = False

move bmap elves start end turn steps
    | done = turn + 1
    | otherwise = move bmap next_elves start end (turn + 1) steps
    where
        ((_, _), (height, width)) = bounds elves
        (sy, sx) = start
        next_elves = next_emap bmap elves sx sy turn
        done = (next_elves ! end ) || (turn >= steps)

main = do
    input_list <- fmap Text.lines (Text.readFile "../../data/day24.txt")
    let valley_input = init (drop 1 (map(\line -> init (drop 1 (Text.unpack line))) input_list))
    let (elves, blizzard_map) = parse valley_input
    part1 elves blizzard_map
    part2 elves blizzard_map

part1 elves blizzard_map = do
    let ((_, _), (height, width)) = bounds elves
    let goal = (height, width)
    let res = move blizzard_map elves (0, 0) goal 1 1000
    print res

part2 elves blizzard_map = do
    let ((_, _), (height, width)) = bounds elves
    let start = (0, 0)
    let end = (height, width)
    let res1 = move blizzard_map elves start end 1 1000
    let res2 = move blizzard_map elves end start (res1 + 1) 1000
    let res = move blizzard_map elves start end (res2 + 1) 1000
    print res
