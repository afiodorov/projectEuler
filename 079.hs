import Prelude hiding (iterate)
import Data.Char (intToDigit)

placeEverywhere :: a -> [a] -> [[a]]
placeEverywhere a [] = [[a]]
placeEverywhere a (x:xs) = [a:x:xs] ++ map ((:) x) (placeEverywhere a xs)

getAllCandidates :: (Ord a) => [a] -> [a] -> [[a]]
getAllCandidates attempt [] = [attempt]
getAllCandidates [] workingPass = [workingPass]
getAllCandidates [x] workingPass = if x `elem` workingPass then [workingPass]
    else placeEverywhere x workingPass
getAllCandidates (x:y:xs) workingPass = do
    candidate <- getAllCandidates (y:xs) workingPass
    if successFullAttempt (x:y:xs) candidate then return candidate
    else
        let (afterY, beforeY) = break ((==) y) (reverse candidate) in
        let putBefore = map (flip (++) (y:(reverse afterY))) $ placeEverywhere x (init (reverse beforeY)) in
        filter (successFullAttempt (x:y:xs)) (putAfter candidate) ++ putBefore
        where
            putAfter candidate = if x `elem` candidate then
                let (beforeX, afterX) = break ((==) x) candidate in
                    map ((++) beforeX) $ placeEverywhere y afterX
            else []


successFullAttempt :: (Ord a) => [a] -> [a] -> Bool
successFullAttempt _ [] = False
successFullAttempt [] _ = True
successFullAttempt [x] xs = x `elem` xs
successFullAttempt (x:y:xs) candidate = successFullAttempt (y:xs) (tail' (dropWhile (/= x) candidate))
    where
        tail' [] = []
        tail' ys = tail ys

iterate :: (Ord a) => [[a]] -> [a] -> [[a]]
iterate workingPasses attempt = do
    workingPass <- workingPasses
    getAllCandidates attempt workingPass

keepListsWithMinLength :: [[a]] -> [[a]]
keepListsWithMinLength a = filter (\b -> length b == minL) a
    where
        minL = minLength a
        minLength :: [[a]] -> Int
        minLength [a] = length a
        minLength (x:xs) = min (length x) (minLength xs)


main :: IO()
main = print $ map (map intToDigit) $ foldl (\pass -> keepListsWithMinLength . (iterate pass)) [head input] input
input :: [[Int]]
input = [
    [3, 1, 9],
    [6, 8, 0],
    [1, 8, 0],
    [6, 9, 0],
    [1, 2, 9],
    [6, 2, 0],
    [7, 6, 2],
    [6, 8, 9],
    [7, 6, 2],
    [3, 1, 8],
    [3, 6, 8],
    [7, 1, 0],
    [7, 2, 0],
    [7, 1, 0],
    [6, 2, 9],
    [1, 6, 8],
    [1, 6, 0],
    [6, 8, 9],
    [7, 1, 6],
    [7, 3, 1],
    [7, 3, 6],
    [7, 2, 9],
    [3, 1, 6],
    [7, 2, 9],
    [7, 2, 9],
    [7, 1, 0],
    [7, 6, 9],
    [2, 9, 0],
    [7, 1, 9],
    [6, 8, 0],
    [3, 1, 8],
    [3, 8, 9],
    [1, 6, 2],
    [2, 8, 9],
    [1, 6, 2],
    [7, 1, 8],
    [7, 2, 9],
    [3, 1, 9],
    [7, 9, 0],
    [6, 8, 0],
    [8, 9, 0],
    [3, 6, 2],
    [3, 1, 9],
    [7, 6, 0],
    [3, 1, 6],
    [7, 2, 9],
    [3, 8, 0],
    [3, 1, 9],
    [7, 2, 8],
    [7, 1, 6] ]
