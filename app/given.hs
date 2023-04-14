import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int

-- A parallel map using par and pseq
pmap :: (a->b) -> [a] -> [b]
pmap _ [] = []
--pmap f (x:xs) = fx `par` fxs `pseq` (fx : fxs)
pmap f (x:xs) = pseq (par fx fxs) (fx : fxs)
    where 
        fx = f x
        fxs = pmap f xs

-- A parallel map using rpar
rmap :: (a -> b) -> [a] -> Eval [b]
rmap _ [] = return []
rmap f (a:as) = do
  b <- rpar (f a)
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

----------------------------------------------------------
-- Merge sort using parallelization

-- A merge sort algorithm
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let (left, right) = split' xs
    in merge (mergeSort left) (mergeSort right)

-- A function that splits a list on the middle
split' :: [a] -> ([a], [a])
split' xs = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- A higher order parallel divide and conquer function
divideAndConquer :: ([a] -> Bool) -> ([a] -> b) -> ((b,b) -> b) -> ([a] -> ([a],[a])) -> [a] -> Par b
divideAndConquer baseCase solve combine divide xs =
    if baseCase xs
        then return $ solve xs
        else do
            let (left, right) = divide xs
            leftResult  <- spawn $ divideAndConquer baseCase solve combine divide left
            rightResult <- spawn $ divideAndConquer baseCase solve combine divide right
            leftVal  <- get leftResult
            rightVal <- get rightResult
            return $ combine (leftVal, rightVal)


-- A parallel merge sort algorithm utilizing our higher order function
pMergeSort :: (Ord a) => [a] -> [a]
pMergeSort = runPar . divideAndConquer baseCase solve combine divide
  where
   baseCase [x] = True
   baseCase _ = False
   solve [x] = [x]
   combine (a,b) = merge a b
   divide = split'

---------------------------------------------------------------
-- Sudoku solverprep and map functions with strategies

import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

someStratMap :: NFData b => (a -> b) -> [a] -> [b]
someStratMap f xs = map f xs `using` parBuffer rdeepseq

--someStratMap :: NFData b => (a -> b) -> [a] -> [b]
--someStratMap f xs = map f xs `using` parList rdeepseq

--someStratMap :: NFData b => (a -> b) -> [a] -> [b]
--someStratMap f xs = map f xs `using` parListChunk rdeepseq

-------------------------------------------------------------------------

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
rJackknife :: ([a] -> b) -> [a] -> Eval [b]
rJackknife f = rmap f . resamples 500

-- Jackknife using parMap (built in). Can not be run with par monad imported
--parJackknife :: NFData b => ([a] -> b) -> [a] -> [b]
--parJackknife f xs = parMap rdeepseq f (resamples 500 xs)

-- Jackknife using smap (strategies implementation)
sJackknife :: NFData b => ([a] -> b) -> [a] -> [b]
sJackknife f = smap f . resamples 500

-- Jackknife using pmmap (par monad implementation)
pmJackknife :: NFData b => ([a] -> Par b) -> [a] -> Par [b]
pmJackknife f = pmmap f . resamples 500


crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

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
  putStrLn $ "Mergesort output:  " ++ show (pMergeSort [3,1,6])

  -- Sudoku thing
  file <- readFile "./app/sudoku.txt"

  let puzzles   = lines file
      solutions = runEval (someStratMap solve puzzles)

  evaluate (length puzzles)
  print (length (filter isJust solutions))

  defaultMain
        [ bench "jackknife" (nf (jackknife  mean) rs)
        , bench "pJackknife" (nf (pJackknife  mean) rs)
        , bench "rJackknife" (nf (runEval . rJackknife  mean) rs)
        , bench "parJackknife" (nf (parJackknife mean) rs)
        , bench "sJackknife" (nf (sJackknife mean) rs)
        , bench "pmJackknife" (nf (runPar . pmJackknife (return . mean)) rs)
         ]