import Data.Numbers.Primes (primes)
import Data.List (nub, intersect, sort, permutations)
import Data.Digits (digits, unDigits)
import Control.Monad (replicateM)

candidates = let myprimes  = (dropWhile (<1000) $ takeWhile (<10000) primes)
                 genPerms = map (unDigits 10) . permutations . digits 10
                 perms = map ((myprimes `intersect`) . nub . genPerms) myprimes
             in nub . map sort $ filter ((> 3) . length) perms

answers = let isArithmetic x = head x + x !! 2 == 2 * (x !! 1)
              areElemnsUniq x =  head x /= x !! 2
              choose3 = concatMap (map sort . replicateM 3)
              isTrulyArith x = isArithmetic x && areElemnsUniq x
          in filter isTrulyArith $ nub $ choose3 candidates

main :: IO ()
main = putStrLn $ concatMap show (last answers)
