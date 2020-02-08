{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Sudoku.Sudoku where
import Control.Monad (guard, when, join)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Data.STRef
import Sudoku.Internal
import System.Random
import qualified  Data.Set as Set

type Board = UArray (Int, Int) Int -- +-+-+-+
                                  -- |1|2|3|
                                  -- +-----+
                                  -- |4|5|6|
                                  -- +-----+
                                  -- |7|8|9|
                                  -- +-+-+-+

type ScanArea = ([Int], [Int], [Int]) -- Rows to scan, colums to scan, blocks

type Possibility = ((Int, Int), Int)

sudokuSz :: Int
sudokuSz = 9

blockWidth :: Int
blockWidth = 3

scanAll :: ScanArea
scanAll = let !r = [1..sudokuSz] in (r, r, r)

-- | Return the index of the coordinate's block
getBlock :: (Int, Int) -> Int
getBlock (i, j) = i' + j' where
    i' = (i - 1) `div` blockWidth * blockWidth + 1
    j' = (j - 1) `div` blockWidth

-- | Return all indicies of a given block
blockIndices :: Int -> [(Int, Int)]
blockIndices n = is `distrubute` js where
    distrubute is' js' = [ (i, j) | i <- is', j <- js' ]
    is = let del = (n - 1) `div` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]
    js = let del = (n - 1) `mod` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]

-- | Return all indicies of a given row
rowIndices :: Int -> [(Int, Int)]
rowIndices i = [(i, j) | j <- [1 .. sudokuSz]]

-- | Return all indicies of a given column
colIndices :: Int -> [(Int, Int)]
colIndices j = [(i, j) | i <- [1 .. sudokuSz]]

-- | Return list of conflict indices TODO tidy
canidate :: [Int] -> Bool -> [(Int, [Int])]
canidate xs z = assocs $ runSTArray do
    arr <- newArray (0, sudokuSz) [] :: ST s (STArray s Int [Int])
    i <- newSTRef xs
    j <- newSTRef 1
    let appIdx d ix = do
            tmp <- readArray arr d
            writeArray arr d $ ix:tmp
    let inc to = do
            modifySTRef j (+ 1)
            modifySTRef i tail
            to
    fix $ \go -> do
        i' <- readSTRef i
        j' <- readSTRef j
        case i' of
            [] -> return ()
            (0:_) -> if z then appIdx 0 j' >> inc go else inc go
            (u:_) -> appIdx u j' >> inc go
    return arr

blockIdxCrd :: Int -> Int -> (Int, Int)
blockIdxCrd b idx = (i, j) where
    (dI, dJ) = divMod (idx - 1) blockWidth
    (i', j') = let (i'', j'') = divMod (b - 1) blockWidth
               in (i'' * 3, j'' * 3)
    (i, j)   = (dI + i' + 1, dJ + j' + 1)

conflicts :: Board -> Int -> Int -> Int -> IO [(Int, Int)]
conflicts b x i j = let
    f = flip canidate False
    (a, z, c) = unitVals b $ pointUnit (i, j) :: ([Int], [Int], [Int])
    (a', z', c') = (f a, f z, f c)
    in do
        putStrLn "a::"
        print a
        putStrLn "z::"
        print b
        putStrLn "c::"
        print c
        putStr "args: "
        print (x, i, j)
        let a_ = g ((i,) <$>) a'
        let z_ = g ((,j) <$>) z'
        let c_ = g (blockIdxCrd (getBlock (i, j)) <$>) c'
        putStr "row conflicts:\n"
        putStrLn . unlines $ (\x -> "   " <> show x <> "\n") <$> a'
        putStr "column conflicts:\n"
        putStrLn . unlines $ (\x -> "   " <> show x <> "\n") <$> z'
        putStr "block conflicts: "
        putStrLn . unlines $ (\x -> "   " <> show x <> "\n") <$> c'
        putStr "final block output: "
        putStrLn . unlines $ (\x -> "   " <> show x <> "\n") <$> c_
        print $  a_ <> z_ <> c_
        return $ a_ <> z_ <> c_
        where
            g u us = join $ flip mapMaybe us \(d, xs) -> if d == x
                then if (length . take 2 $ xs) > 1
                    then Just $ u xs
                    else Nothing
                else Nothing

-- | Return true if the area scanned over could potentially form part of a valid
-- sudoku solution
validateArea :: ScanArea -> Board -> Bool -> Bool
validateArea (row, col, blk) g z = and $ map f r <> map f c <> map f b where
    validate k xs = map (g !) . k <$> xs
    f = all (\x -> (length . take 2 . snd $ x) <= 1) . (flip canidate z)
    r = validate rowIndices row
    c = validate colIndices col
    b = validate blockIndices blk

-- | Find all indices in the grid with a given value
findIndices :: Int -> Board -> [(Int, Int)]
findIndices n arr = map fst . filter (\x -> snd x == n) . assocs $ arr

-- | Produces the list of all values over a list of indices given a generator
-- and seed.
fetch :: Board -> (Int -> [(Int, Int)]) -> Int -> [Int]
fetch g f n = (g !) <$> f n

type Prune = (Possibility -> Bool)

-- | Creates a function that returns true if a point is in the same row, col, or
-- block as the provided point
pruneDepends :: Possibility -> Prune
pruneDepends (c, v) = \(c', v') -> if v == v'
    then fst c == fst c'
        || snd c == snd c'
        || getBlock c == getBlock c'
    else False

-- | Composition of pruning predicates under OR
composePrune :: Prune -> Prune -> Prune
composePrune a b = \x -> b x || a x

-- discards branches of invalid posibilties using prunes. (like a dfs over a
-- multitree)
-- | Generate all possible solutions
forest :: [((Int, Int), [Int])] -> [[Possibility]]
forest [] = []
forest ((c', vs') : xs') = map (map fst)
    $ f xs' [[((c', v'), pruneDepends (c', v'))] | v' <- vs'] where
        f [] acc = acc
        f ((c, vs) : xs) acc = f xs [ g (c, v) u x
            | (x@((_,u):_)) <- acc, v <- vs]
        g p u xs = if u p
            then []
            else (p, composePrune (pruneDepends p) u) : xs

-- | Returns (row, column, and block) index of a given point
pointUnit :: (Int, Int) -> (Int, Int, Int)
pointUnit x = (fst x, snd x, getBlock x)

-- | Takes a list unit indexes (pointUnit) and returns the
-- values of those sectors
unitVals :: Board -> (Int, Int, Int) -> ([Int], [Int], [Int])
unitVals g (a, b, c) = ( fetch g rowIndices a
                       , fetch g colIndices b
                       , fetch g blockIndices c )

-- | Returns all mising values in a sudoku unit vector
missingVals :: ([Int], [Int], [Int]) -> [Int]
missingVals (a, b, c) = Set.toList $ Set.difference full some where
    full = Set.fromList [1 .. sudokuSz]
    some = Set.unions $ Set.fromList <$> [a, b, c]

-- | Returns all possible solutions
solve :: Board -> StdGen -> Maybe [[Possibility]]
solve g rand
    | validateArea scanAll g False = let
        is = findIndices 0 g
        -- Shuffling the seed dec time on avg
        f a b = (a, fst $ shuffle' (missingVals b) rand)
        -- Possible solutions at each index
        rs = zipWith f is $ unitVals g . pointUnit <$> is
        result = if null is
            then (True, [])
            else (False,)  $ do
                sltn <- forest rs :: [[Possibility]]
                let test' = g  // sltn
                guard $ validateArea scanAll test' True
                return sltn
        in case result of -- handle no result found case
            (True, x) -> Just x
            (_,  [])  -> Nothing
            (_,  xs)  -> Just xs
    | otherwise = Nothing

test :: [Int]
test = [ 7, 2, 6, 4, 9, 3, 8, 1, 5
       , 3, 1, 5, 7, 2, 8, 9, 4, 6
       , 4, 0, 9, 6, 5, 1, 2, 3, 7
       , 8, 5, 2, 1, 4, 7, 6, 9, 3
       , 6, 7, 3, 9, 8, 5, 1, 2, 4
       , 9, 4, 1, 3, 6, 2, 7, 5, 8
       , 1, 9, 4, 8, 3, 6, 5, 7, 2
       , 5, 6, 7, 2, 1, 4, 3, 8, 9
       , 2, 3, 8, 5, 7, 9, 4, 6, 1 ]
