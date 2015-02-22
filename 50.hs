import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.List (tails, maximumBy)
import Data.Function (on)

primes :: Int -> [Int]
primes n = sieve $ 2 : [3,5..n]
    where sieve :: [Int] -> [Int]
          sieve [] = []
          sieve l@(x:xs)
              | x*x >= n = l
              | otherwise = x : sieve list'
              where list' = filter (\y -> y `rem` x /= 0) xs

longestSum :: Int -> [Int] -> (Int, Int)
longestSum threshold list = let
                    ans = either id pick2 (longestSum' threshold list)
                    pick2 (a,  b, _,  _) = (a, b)
                  in
                  ans

longestSum' :: Int -> [Int] -> Either (Int, Int) (Int, Int, Int, Int)
longestSum' threshold list = foldM f (0, 0, 0, 0) list
    where
        f (numPrimes, lastPrime, len,  sum') prim = if newSum < threshold
            then Right (numPrimes', newPrime, len + 1, newSum)
            else Left (numPrimes, lastPrime)
            where
            newSum = sum' + prim
            found = newSum `elem` list
            newPrime = if found then newSum else lastPrime
            numPrimes' = if found then len + 1 else numPrimes

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs

main :: IO ()
main = do
    threshold <- getIntArg
    let firstPrimes = primes threshold
        sublists = (init . tails $ firstPrimes)
        chains = map (longestSum threshold) sublists
        (_, ans) = maximumBy (compare `on` fst) chains
        in
        print ans
