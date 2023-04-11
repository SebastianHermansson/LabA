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

-- A parallel map using strategies
smap :: NFData b => (a -> b) -> [a] -> [b]
smap f xs = map f xs `using` parList rdeepseq

-- A parallel map using par monad
pmmap :: NFData b => (a -> Par b) -> [a] -> Par [b]
pmmap f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

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
sJackknife f xs = smap f (resamples 500 xs)

-- Jackknife using pmmap (par monad implementation)
pmJackknife :: NFData b => ([a] -> Par b) -> [a] -> Par [b]
pmJackknife f xs = pmmap f (resamples 500 xs)


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
  defaultMain
        [ bench "jackknife" (nf (jackknife  mean) rs)
        , bench "pJackknife" (nf (pJackknife  mean) rs)
        , bench "rJackknife" (nf (runEval . rJackknife  mean) rs)
        --, bench "parJackknife" (nf (parJackknife mean) rs)
        , bench "sJackknife" (nf (sJackknife mean) rs)
        , bench "pmJackknife" (nf (runPar . pmJackknife (return . mean)) rs)
         ]