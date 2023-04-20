import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Control.DeepSeq

import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int

-- A parallel map using par and pseq
pmap :: (a->b) -> [a] -> [b]
pmap _ [] = []
--pmap f (x:xs) = fx `par` fxs `pseq` (fx : fxs)
pmap f (x:xs) = pseq (par fx fxs) (fx : fxs)
    where 
        fx = force f x
        fxs = pmap f xs

-- A parallel map using rpar
rmap :: (NFData b) => (a -> b) -> [a] -> Eval [b]
rmap _ [] = return []
rmap f (a:as) = do
  b <- rpar (force $ f a)
  bs <- rmap f as
  return (b:bs)

-- A parallel map using parList strategy
smap :: NFData b => (a -> b) -> [a] -> [b]
smap f xs = map f xs `using` parList rdeepseq

-- A parallel map using par monad
pmmap :: NFData b => (a -> Par b) -> [a] -> Par [b]
pmmap f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

------------------------------------------------------------

mean :: (RealFrac a) => [a] -> a
mean = do fini . foldl' go (T 0 0) 
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1

resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

-- Regular jackknife
jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

-- Jackknife using pmap (par and pseq implementation)
pJackknife :: ([a] -> b) -> [a] -> [b]
pJackknife f = pmap f . resamples 500

-- Jackknife using Eval monad
rJackknife :: (NFData b) => ([a] -> b) -> [a] -> Eval [b]
rJackknife f = rmap f . resamples 500

---- Jackknife using parMap (built in). Can not be run with par monad imported
--parJackknife :: NFData b => ([a] -> b) -> [a] -> [b]
--parJackknife f xs = parMap rdeepseq f (resamples 500 xs)

-- Jackknife using smap (strategies implementation)
sJackknife :: NFData b => ([a] -> b) -> [a] -> [b]
sJackknife f = smap f . resamples 500

-- Jackknife using pmmap (par monad implementation)
pmJackknife :: NFData b => ([a] -> Par b) -> [a] -> Par [b]
pmJackknife f = pmmap f . resamples 500


crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

----------------------------------------------------------
-- Divide and conquer

-- A mergesort function

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge2 (mergesort left) (mergesort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] ys = ys
merge2 xs [] = xs
merge2 (x:xs) (y:ys)
  | x <= y    = x : merge2 xs (y:ys)
  | otherwise = y : merge2 (x:xs) ys

-- A function that splits a list on the middle and returns a list of those lists

split' :: [a] -> [[a]]
split' [] = [[], []]
split' xs = [take n xs, drop n xs]
  where n = (length xs + 1) `div` 2

-- A function that merges two lists in order
merge :: Ord a => [[a]] -> [a]
merge [[], []] = []
merge [[], ys] = ys
merge [xs, []] = xs
merge ((x:xs):(y:ys):z)
    | x < y     = x : merge [xs,(y:ys)]
    | otherwise = y : merge [(x:xs), ys]


-- A higher order parallel divide and conquer algorithm
divideAndConquer :: (NFData b, NFData a) => (a -> Bool) -> (a -> [a]) -> (a -> b) -> ([b] -> b) -> a -> b
divideAndConquer shouldDivide divide solve combine input
  | shouldDivide input = combine $ runEval . parList rdeepseq $ map (divideAndConquer shouldDivide divide solve combine) (divide input)
  | otherwise          = solve input

-- A parallel merge sort algorithm utilizing our higher order function
pMergeSort :: (NFData a, Ord a) => [a] -> [a]
pMergeSort = divideAndConquer baseCase divide solve combine 
  where
   baseCase x |length(x)<8 = False
              |otherwise   = True
   solve xs = sort xs
   combine xs = merge xs
   divide = split'

-- A parallel sum function utilizing our higher order function
pSum :: (Num a, NFData a) =>[a] -> a
pSum = divideAndConquer baseCase divide solve combine 
  where
   baseCase x |length(x)<100 = False
              |otherwise   = True
   solve xs = sum xs
   combine (x:y:_) = x + y
   divide = split'
---------------------------------------------------------------
-- Sudoku map functions with strategies

parBufferMap :: NFData b => (a -> b) -> [a] -> [b]
parBufferMap f xs = map f xs `using` parBuffer 50 rdeepseq

parListMap :: NFData b => (a -> b) -> [a] -> [b]
parListMap f xs = map f xs `using` parList rdeepseq

parListChunkMap :: NFData b => (a -> b) -> [a] -> [b]
parListChunkMap f xs = map f xs `using` parListChunk 200 rdeepseq

-------------------------------------------------------------------------

main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)

  -- Merge sort test
  putStrLn $ "Mergesort output:  " ++ show (mergesort rs)
  putStrLn $ "Parallel mergesort output:  " ++ show (pMergeSort rs)

  -- Sum test
  putStrLn $ "Sum output:  " ++ show (sum rs)
  putStrLn $ "Parallel sum output:  " ++ show (pSum rs)

  -- Sudoku solver using three types of parallelization
  file <- readFile "./app/sudoku17.16000.txt"

  let puzzles   = lines file

  print(map solve puzzles)
  print(parBufferMap solve puzzles)
  print(parListMap solve puzzles)
  print(parListChunkMap solve puzzles)

  defaultMain
        [ 
          bench "jackknife" (nf (jackknife  mean) rs)
        , bench "pJackknife" (nf (pJackknife  mean) rs)
        , bench "rJackknife" (nf (runEval . rJackknife  mean) rs)
--        , bench "parJackknife" (nf (parJackknife mean) rs)
        , bench "sJackknife" (nf (sJackknife mean) rs)
        , bench "pmJackknife" (nf (runPar . pmJackknife (return . mean)) rs)
        , bench "mergesort" (nf (mergesort) rs)
        , bench "pmergesort" (nf (pMergeSort) rs)
        , bench "sum" (nf (sum) rs)
        , bench "psum" (nf (pSum) rs)
        , bench "sudoku/map" (nf (map solve) puzzles)
        , bench "sudoku/parbuffer" (nf (parBufferMap solve) puzzles)
        , bench "sudoku/parlist" (nf (parListMap solve) puzzles)
        , bench "sudoku/parlistchunk" (nf (parListChunkMap solve) puzzles)
         ]