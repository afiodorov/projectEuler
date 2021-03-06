import Control.Monad (msum)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.List (nub)
import Memoize (memoize)
import Data.Functor ((<$))

smallestInt :: Int -> Int
smallestInt size = fromJust . msum $ map keepIfDecomposable [size ..]
    where
    keepIfDecomposable n = n <$ decompose size n

decompose :: Int -> Int -> Maybe [Int]
decompose size n = decompose' size n n

decompose' :: Int -> Int -> Int -> Maybe [Int]
decompose' size prod sum'
    | sum' < size               = Nothing
    | prod == 1 && size == sum' = Just (replicate size 1)
    | prod == 1 && size /= sum' = Nothing
    | otherwise                 = msum $ map f (memoize factors prod)
    where
        f d = (d:) <$> decompose' (size - 1) (prod `div` d) (sum' - d)

factors :: Int -> [Int]
factors n = lower ++ upper'
    where
    lower = filter ((0 ==) . mod n) (takeWhile (<= maxFactor) [2..])
    maxFactor = truncate (sqrt (fromIntegral n :: Double))
    upper = map (div n) (reverse lower) ++ [n]
    upper' = if maxFactor == head upper then tail upper else upper


main :: IO ()
main = print $ sum . nub $ map smallestInt [2..12000]
