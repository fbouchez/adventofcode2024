{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import AoC
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.ST
-- import qualified Data.Algebra.Boolean as B
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Array
import Data.Array.ST
import Data.Tuple.Extra
import Data.Graph
import Data.Set (Set)
import qualified Data.Set as Set

-- import GHC.Utils.Misc

import Debug.Trace
import qualified Data.Text as T
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP hiding (count)
import Text.Printf
-- import Text.Scanf



-- If part1 and part2 are very different,
-- toggle which part to compute with this flag.
part2 = False


-- Check if the position is an obstacle ('#')
isObstacle :: Coord -> CharMap -> Bool
isObstacle pos grid = grid!pos == '#'

-- Simulate the guard's path and collect visited positions
simulateGuard :: CharMap -> Coord -> Cardir -> Set Coord -> Set Coord
simulateGuard grid pos dir visited
  | not (inBounds grid nextPos) = visited -- Guard moves out of bounds
  | isObstacle nextPos grid       = simulateGuard grid pos (turnRight dir) visited -- Obstacle encountered, turn right
  | otherwise                     = simulateGuard grid nextPos dir (Set.insert nextPos visited) -- Move forward
  where
    nextPos = move pos dir

-- Parse the grid to find the guard's initial position and direction
findGuard :: CharMap -> Maybe (Coord, Cardir)
findGuard grid = foldl' findFold Nothing $ assocs grid
  where findFold (Just d) (_, _) = Just d
        findFold Nothing (_, '.') = Nothing
        findFold Nothing (_, '#') = Nothing
        findFold Nothing (pos, c) = Just $ (pos, chDir c)

-- Main function to calculate the number of distinct positions visited
countVisitedCoords :: CharMap -> Int
countVisitedCoords grid =
  let ((startX, startY), dir) = fromJust $ findGuard grid
      visited = simulateGuard grid (startX, startY) dir (Set.singleton (startX, startY))
  in Set.size visited

-- Example usage
main :: IO ()
main = do
  -- let grid = [ "....#.....",
               -- ".........#",
               -- "..........",
               -- "..#.......",
               -- ".......#..",
               -- "..........",
               -- ".#..^.....",
               -- "........#.",
               -- "#.........",
               -- "......#..." ]
  grid <- getCharMap
 
  print $ countVisitedCoords grid
