import System.Environment (getArgs)

primes :: [Int]
primes = sieve [2..]
    where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

main :: IO ()
main = do
    (whichPrime:_) <- getArgs
    let n = read whichPrime :: Int in
        print $ primes !! (n - 1)
