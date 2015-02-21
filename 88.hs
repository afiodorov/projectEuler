import Control.Monad (msum)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.List (nub)
import Memoize (memoize)

lowerBound :: (Integral a) => a -> a
lowerBound n = ceiling $ m ** (m / (m - 1))
    where
        m = fromIntegral n :: Double

smallestInt :: Int -> Int
smallestInt size = fromJust . msum $ map f [lowerBound size ..]
    where
    f n = case decompose size n of
        Nothing -> Nothing
        Just _  -> Just n

decompose :: Int -> Int -> Maybe [Int]
decompose size n = decompose' size n n

decompose' :: Int -> Int -> Int -> Maybe [Int]
decompose' size prod sum'
    | prod == 1 && size == sum' = Just (replicate size 1)
    | prod == 1 && size /= sum' = Nothing
    | otherwise                 = msum $ map f (memoize factors prod)
    where
        f d = (d:) <$> decompose' (size - 1) (prod `div` d) (sum' - d)

factors :: Int -> [Int]
factors n = filter ((0 ==) . mod n) (takeWhile (<= n) [2..])

main :: IO ()
main = print $ sum . nub $ map smallestInt [2..12000]
