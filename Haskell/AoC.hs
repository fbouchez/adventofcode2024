{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AoC where

import System.IO
import System.CPUTime
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Exception.Base
import Debug.Trace
import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Ix
import Data.Graph
import qualified Data.HashMap as H
import Data.Hashable
import Data.STRef
import Data.Array
import Data.Array.ST
import Data.Char
import Data.Function
import Data.Tuple.Extra
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as T
import Text.Printf

keepA a b = a
keepB a b = b

cpl a b = (a,b)

couple [a,b] = (a,b)
triple [a,b,c] = (a,b,c)

ap3 f (x,y,z) = (f x, f y, f z)

add2 (x,y) (a,b) = (x+a, y+b)
mul2 p (x,y) = (p*x, p*y)
add3 (x,y,z) (a,b,c) = (x+a, y+b, z+c)

sortFst :: Ord a => [(a,b)] -> [(a,b)]
sortFst = sortBy (compare `on` fst)

groupFst :: Eq a => [(a,b)] -> [(a,[b])]
groupFst = map keepFirst . groupBy ((==) `on` fst)
  where keepFirst l = (a, bl)
          where (al, bl) = unzip l
                a = head al


-- converts a single char digit to its integer value
chrDigit c = assert (isDigit c) $ ord(c) - ord('0')


splitComma :: String -> [String]
splitComma = wordsWhen (==',')

-- get a list of integers separated by ',' on the next line
getInts :: IO [Int]
getInts = do getLine >>= return . splitComma >>= return . map read


-- Count the times indices appear in a list, for indices in a given range
genhist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
genhist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

-- Functions to easily split inputs
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

coupleWhen :: (Char -> Bool) -> String -> (String, String)
coupleWhen p s = let [a,b] = wordsWhen p s in (a,b)


mkArray :: (Ix i) => (i,i) -> (i -> a) -> Array i a
mkArray bnds f = array bnds $ map (\i -> (i, f i)) $ range bnds


type Coord = (Int, Int)
type DigitMap = Array Coord Int
type CharMap = Array Coord Char
type BoolMap = Array Coord Bool

type XYZCoord = (Int, Int, Int)

getCharMap :: IO CharMap
getCharMap = do
    contents <- getContents
    let rows = lines contents
        height = length rows
        width  = length . head $ rows
    let imap = listArray ((1,1), (height,width)) $ concat rows
    return imap

-- reads from stdin a 2d array of boolean values
-- takes as argument a function converting a char into a bool
-- (e.g., '0' -> false, '1' -> true)
getBoolMap :: (Char -> Bool) -> IO BoolMap
getBoolMap chBool = do
    contents <- getContents
    let rows = lines contents
        height = length rows
        width  = length . head $ rows
    let imap = listArray ((1,1), (height,width)) $ map chBool $ concat rows
    return imap




getIntMap :: IO DigitMap
getIntMap = do
    contents <- getContents
    let lns = lines contents
        height = length lns
        width  = length . head $ lns
    let imap = listArray ((1,1), (height,width)) $ map digitToInt $ join lns
    return imap


showDigitMap :: DigitMap -> String
showDigitMap imap =
    let ((miny, minx), (maxy, maxx)) = bounds imap
        makeline y = join . map (\x -> show $ imap!(y,x)) $ [minx..maxx]
        rows = map (\y -> makeline y) [miny..maxy]
    in unlines rows


showCharMap grid = unlines $ map getRow $ [lor..hir]
  where ((lor,loc),(hir,hic)) = bounds grid
        getRow r = map (getPos r) $ [loc..hic]
        getPos r c = grid!(r,c)

showBoolMap grid = unlines $ map getRow $ [lor..hir]
  where ((lor,loc),(hir,hic)) = bounds grid
        getRow r = map (getPos r) $ [loc..hic]
        getPos r c = if grid!(r,c) then 'O' else '.'



data Turn = L | R deriving (Eq, Show)
data Cardir = N | S | W | E deriving (Eq, Show)

chDir '^' = N
chDir 'v' = S
chDir '<' = W
chDir '>' = E

dirCh N = '^'
dirCh S = 'v'
dirCh W = '<'
dirCh E = '>'

opposite N = S
opposite S = N
opposite E = W
opposite W = E

inDir N = (-1, 0)
inDir S = (1, 0)
inDir W = (0, -1)
inDir E = (0, 1)

lookIn N = [(-1,-1), (-1, 0), (-1, 1)]
lookIn S = [(1,-1), (1, 0), (1, 1)]
lookIn W = [(-1,-1), (0, -1), (1, -1)]
lookIn E = [(-1,1), (0, 1), (1, 1)]

moveTo dir p = add2 p $ inDir dir

crossDirs :: [(Coord)]
crossDirs = [(1,0), (-1,0), (0,1), (0,-1)]

crossLocalDirs :: [(Coord)]
crossLocalDirs = [(1,0), (-1,0), (0,0), (0,1), (0,-1)]

localArea = (,) <$> [-1,0,1] <*> [-1,0,1]
aroundDirs = filter (/=(0,0)) $ localArea

neighFuns :: [Coord] -> [Coord -> Coord]
neighFuns dirs = map add2 dirs


neighDir dirs (r,c) = map ($ (r,c)) $ neighFuns dirs

crossNeighbours = neighDir crossDirs
allNeighbours = neighDir aroundDirs

neighb :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
neighb arr (r,c) = filter (inRange (bounds arr)) $ allNeighbours (r,c)

neighbounds bnds (r,c) = filter (inRange bnds) $ allNeighbours (r,c)


convertBin :: [Int] -> Int
convertBin = aux 0
  where aux acc [] = acc
        aux acc (v:rp) = aux (2*acc+v) rp



-- Do a foldl until a certain condition is met,
-- f checks the conditions and return a Left when it is not needed to 
-- continue further
-- If a Left is never returned, apply notfound function to the Right
--
-- foldlUntil f acc = either id (\_ -> error "Left never returned") . (foldM f acc)
foldlUntil :: Foldable t => (b -> a -> Either c b) -> (b -> c) -> b -> t a -> c
foldlUntil f nf acc l = either id nf . (foldM f acc) $ l

hist :: (Ix a, Num b) => [a] -> Array a b
hist is = genhist (low,high) is
  where low = minimum is
        high = maximum is

lookup' :: (Eq a) => a -> [(a,b)] -> b
lookup' e l = fromJust $ lookup e l


-----------------------
--
-- Parsing
--
-----------------------

-- a regular word containing only alphabetic characters
word :: T.ReadP String
word = T.many1 $ T.satisfy isAlpha


-- parses an classic "identifier" in programming languages
parseIdent :: T.ReadP String
parseIdent = do
    c <- T.satisfy isAlpha
    rst <- T.many $ T.satisfy isAlphaNum
    return $ c:rst


-- an alphanumeric number, or '.' or '/'
isAlnumDot c =
    isAlphaNum c || c == '.' || c == '/'


-- parse a direction NSEW that uses "arrows": ^v><
parseDir :: T.ReadP Cardir
parseDir = do
    c <- T.char '<' <|> T.char '>' <|> T.char 'v' <|> T.char '^'
    return $ chDir c

-- parse 'on/off' switches
parseOnOff :: T.ReadP Bool
parseOnOff = do
    s <- T.string "on" <|> T.string "off"
    return $ s == "on"

-- parses an number
number :: T.ReadP Int
number = read <$> numberStr

-- parses a string that represents a number
numberStr :: T.ReadP String
numberStr = do
    neg <- T.option ' ' (T.satisfy (=='-'))
    num <- T.many1 (T.satisfy isDigit)
    return (neg:num)

-- parses a list of number
numberList :: T.ReadP [Int]
numberList = T.sepBy1 number commaSep


-- parses just a line return
lineReturn = T.satisfy (=='\n')

doubleLineReturn = lineReturn >> lineReturn

-- separator: a char followed by spaces
charSep c = T.char c >> T.skipSpaces

commaSep = charSep ','
colonSep = charSep ':'
semicolSep = charSep ';'

-- generates a parser for a range of integer
-- `str` is the string used for separating the two values
genRange :: String -> T.ReadP (Int, Int)
genRange str = do
    lo <- number
    T.string str
    hi <- number
    return (lo, hi)


parseCoords :: T.ReadP (Coord)
parseCoords = genRange ","

-- parses a range such as "2..10"
dotRange :: T.ReadP (Int, Int)
dotRange = genRange ".."

-- parses a range such as "2-10"
dashRange :: T.ReadP (Int, Int)
dashRange = genRange "-"


-- parses something significant followed by something insignificant
closedBy p f = do
    x <- p
    f
    return x

-- parses either a finishing line return or not, then the end-of-file
endOfInput = (T.optional lineReturn) >> T.eof


parseContents parser = getContents >>= applyParser parser

-- gets a parser and apply it to the contents in stdin
-- returning the first match
applyParser parser contents = do
    let dat = T.readP_to_S (parser `closedBy` endOfInput) contents
    return $ fst . head $ dat


-----------------------
--
-- Graph manipulations
--
-----------------------



-- Edge in lexicographic order
ed :: Edge -> Edge
ed (x,y) = if x < y then (x,y) else (y,x)

edge :: Vertex -> Vertex -> Edge
edge x y = ed (x,y)

type Weights = Edge -> Int
type Pathslen = (Vertex, Vertex) -> Int

-- Weighted graph
data WGraph = WGraph Graph Weights



extractVerticesNames :: [(String, String, Int)] -> [String]
extractVerticesNames strlinks = nubSort $ ss ++ ts
  where (ss, ts, _) = unzip3 strlinks

-- from a list of names, create two functions
-- that convert names to ints and vice versa
identifyNames names = (nameToInt, intToName)
  where arr = listArray (0, length names - 1) names
        intToName = (arr!)
        hmap = H.fromList $ map swap $ assocs arr
        nameToInt = fromJust . flip H.lookup hmap

convertEdges strlinks nameToInt = map conve strlinks
  where conve (s, t, w) = ((nameToInt s, nameToInt t), w)

makeGenGraph :: Bool -> [(String, String, Int)] -> (WGraph, (String -> Int), (Int ->String))
makeGenGraph direct strlinks =

    let names = extractVerticesNames strlinks
        (ntoi, iton) = identifyNames names
        links = convertEdges strlinks ntoi
    in
    let bothdiredges = e ++ e'
        (e,ws) = unzip links
        e' = fmap swap e
        links' = zip e' ws

        g = buildG (vfirst, vlast) $ if direct then e else bothdiredges
        vfirst = 0
        vlast = length names - 1
    in
    let weight = fromJust . flip H.lookup wmap
          where wmap = H.fromList $ if direct then links else links ++ links'
    in
    (WGraph g weight, ntoi, iton)

makeGraph :: [(String, String, Int)] -> (WGraph, (String -> Int), (Int ->String))
makeGraph = makeGenGraph False

makeDirectedGraph = makeGenGraph True

prettyGenGraph direct (WGraph g w) iton = do
    handle <- openFile "graph.dot" WriteMode

    hPutStrLn handle $ if direct then "digraph {" else "graph {"
    let link = if direct then " -> " else " -- "

    let printEdge e@(s,t) =
          let lab = w e
              ns = iton s
              nt = iton t
          in
          if direct || s <= t
            then hPutStrLn handle $ "\t" ++ ns ++ link ++ nt ++ "[label=" ++ show lab ++ "]"
            else return ()

    mapM_ printEdge $ edges g

    hPutStrLn handle "}"
    hClose handle

prettyGraph = prettyGenGraph False

prettyDirectedGraph = prettyGenGraph True




buildAllPath :: WGraph -> Array (Vertex, Vertex) Int
buildAllPath wg@(WGraph g w) = gridpath
  where
    (vfirst, vlast) = bounds g
    allpaths = concat $ map (buildPaths wg) $ vertices g
    gridpath = array ((vfirst,vfirst), (vlast, vlast)) allpaths


buildPaths :: WGraph -> Vertex -> [((Vertex, Vertex), Int)]
buildPaths (WGraph g w) v = filter (\((v,v'),_) -> v <= v') paths
    where paths = buildPaths' g w v (PQ.singleton 0 v) []


buildPaths' :: Graph -> Weights -> Vertex -> PQ.MinPQueue Int Vertex -> [((Vertex, Vertex), Int)] -> [((Vertex, Vertex), Int)]
buildPaths' g w v prio acc
  | PQ.null prio = acc
  | otherwise = -- trace ("Starting at " ++ show v ++ " current " ++ show v' ++ " accu: " ++ show (sort acc) ++ " neighb " ++ show (neighbours, g!v')) $
        buildPaths' g w v prio' acc'
    where ((dist, v'), priom) = PQ.deleteFindMin prio
          acc' = if notSeen v' then (edge v v', dist) : acc -- need to keep v == v'
                               else acc

          prio' = foldl (\pr n -> PQ.insert (dist + w (v',n)) n pr) priom neighbours
          neighbours = filter notSeen $ g!v'

          notSeen n = Nothing == lookup (edge v n) acc


listReachableVertices (WGraph g w) blocked v =
  reach
  where reach = aux [v] []
        aux [] acc = acc
        aux (f:fs) acc = aux fs' acc'
            where acc' = f:acc
                  fs' = neighbours ++ fs
                  neighbours = filter emptySpot $ filter notSeen $ g!f
                  notSeen n = n `notElem` acc
                  emptySpot n = not $ blocked!n


--
-- A* exploration, starting from an initial state, trying to find the final 
-- state with minimum cost. Return the reversed list of the states in the 
-- minimum path, (first in final state, last in initial state).
-- * next: fonction that given the list of next possible states as well as their cost
-- * cost: gives the cost of a state
-- * heur: heuristic fonction. If given (const 0), then A* is a Dijkstra
--

--
-- A class to represent a state in a problem.
class Pstate a e where
    next :: e -> (Int, a) -> [(Int, a)]
    heuristic :: e -> a -> a -> Int

aStar ::  (Pstate a e, Hashable a, Eq a, Show a) => e -> a -> a -> [(a, Int)]
aStar envir initst finalst = convertSolution $ explore 0 [] hm prio
  where
    hm = H.singleton (hash initst) 0
    prio = PQ.singleton (heuristic envir initst finalst) (0, -1, initst)
      -- priority key is dist w/ heur,
      -- value is a triplet of
      -- * actual distance from initial state
      -- * id of the previous state (0 is the last of the 'prevexplored' list)
        -- * pstate

    -- explore' :: Int -> [(Stat, Int, Int)] -> HashMap WGraph -> Stat -> 
    -- Stat -> (Int, [(Stat, Int, Int)])

    -- nexplored serves as counter, as well as identifier for current state
    explore nexplored prevexplored hm prio =
        -- (if  nexplored `mod` 1000 == 0 
        (if  True
        then trace ("Explored: " ++ show nexplored ++ " -- Current cost: " ++ show dist ++ " (heur: " ++ show distheur ++ ") Current sizes of hm and prio: " ++ show (H.size hm, PQ.size prio)
            ++ "\nExploring state, (previous " ++ show prev ++ ") " ++ show (dist, distheur, st))
        else id) $
        -- trace ("Current sizes of hm and prio: " ++ show (H.size hm, PQ.size prio)) $
        -- trace ("Exploring state, " ++ showCostStat (dist, distheur, st)) $
        -- trace ("Filtered ns state,\n" ++ showNextStats nextsth) $
        if finalreached
          then prevexplored'
          -- else if alreadyseen
                -- then explore' hm prio'' wg finalst
          else explore (nexplored+1) prevexplored' hm' prio'

      where ((distheur, (dist, prev, st)), prioless) = PQ.deleteFindMin prio
            finalreached = st == finalst
            prevexplored' = (st,dist,prev):prevexplored

            nextst = next envir (dist, st)
            nextunseen = filter notSeenLower nextst
            nextstheur = map (\(d, s) -> (d, d + heuristic envir s finalst, s)) nextunseen

            -- inserting all next states in the priority queue
            prio' = foldl insertPrio  prioless nextstheur
              where
                insertPrio acc (d, dh, s) = PQ.insert dh (d, nexplored, s) acc

            -- insert all states that we put in the prio also in the hashmap
            -- if state is already in the map, will update with new cost, which 
            -- is lower
            hm' = foldl (\acc (d, s) -> H.insert (hash s) d acc) hm nextunseen

            -- returns True if s has not already been seen, or has been 
            -- seen with a higher value
            notSeenLower (d, s) = k > d
              where k = H.findWithDefault maxBound (hash s) hm

                -- (if (k < cost) then trace ("Already present with " ++ show (k, cost)) else id) $
                -- (if (k < maxBound && k > cost) then trace ("Already present but value is lower !!! " ++ show (k, cost)) else id) $


    convertSolution :: Show a => [(a, Int, Int)] -> [(a, Int)]
    convertSolution expl =
        let aexpl = listArray (0, length expl - 1) $ reverse expl
            aux (st, dist, prev) acc =
              traceShow (st, dist, prev) $
              let (st', dist', prev') = aexpl!prev
                  acc' = (st, dist):acc
              in
              if prev' == -1 then (st', dist'):acc'
                             else aux (st', dist', prev') acc'
        in
        aux (head expl) []
