import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int

pmap :: (a->b) -> [a] -> [b]
pmap _ [] = []
--pmap f (x:xs) = fx `par` fxs `pseq` (fx : fxs)
pmap f (x:xs) = pseq (par fx fxs) (fx : fxs)
    where 
        fx = f x
        fxs = pmap f xs

--rmap :: (a->b) -> [a] -> [b]
--rmap _ [] = []
--rmap f (x:xs) = runEval $ do 

pMap :: (a -> b) -> [a] -> [b]
pMap f [] = []
pMap f (a:as) = runEval $ do
  b <- rpar (f a)
  bs <- pMap f as
  return (b:bs)


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
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
--jackknife :: ([a] -> b) -> [a] -> [b]
--jackknife f = map f . resamples 500

-- Jackknife using pmap (par and pseq implementation)
--jackknife :: ([a] -> b) -> [a] -> [b]
--jackknife f = pmap f . resamples 500

-- Jackknife using Eval monad
jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = pMap f . resamples 500

-- Jackknife using parMap (built in)
--jackknife :: NFData b => ([a] -> b) -> [a] -> [b]
--jackknife f = parMap rseq f . resamples 500


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
        [
         bench "jackknife" (nf (jackknife  mean) rs)
         ]