import Data.Numbers.Primes (primes)
import Data.List (nub, intersect, sort, permutations)
import Data.Digits (digits, unDigits)
import Control.Monad (replicateM)

candidates = let ps = (dropWhile (<1000) $ takeWhile (<10000) primes) in nub . map sort $ filter ((> 3) . length) $ map ((ps `intersect`) . nub . map (unDigits 10) . permutations . digits 10) ps
answer = filter (\x -> (head x + x !! 2 == 2 * (x !! 1)) && (head x /= x !! 2)) $ nub $ concatMap (map sort . replicateM 3) candidates

main :: IO ()
main = print (answer !! 1)
