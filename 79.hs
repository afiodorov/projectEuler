import Prelude hiding (iterate)
import Data.Char (intToDigit, digitToInt)
import System.Environment (getArgs)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

placeEverywhere :: a -> [a] -> [[a]]
placeEverywhere a [] = [[a]]
placeEverywhere a (x:xs) = (a:x:xs) : map ((:) x) (placeEverywhere a xs)

listsWithXBeforeLastY :: (Ord a) => a -> a -> [a] -> [[a]]
listsWithXBeforeLastY x y list = let
        (afterY', beforeY') = break (y ==) (reverse list)
        (afterY, beforeY) = (reverse afterY', reverse beforeY') in
        map (++ (y : afterY)) $ placeEverywhere x (init beforeY)

listsWithYAfterFirstX :: (Ord a) => a -> a -> [a] -> [[a]]
listsWithYAfterFirstX x y list  = let
        (beforeX, afterX) = break (x ==) list in
        map (beforeX ++) $ placeEverywhere y afterX

getAllCandidates :: (Ord a) => [a] -> [a] -> [[a]]
getAllCandidates attempt [] = [attempt]
getAllCandidates [] workingPass = [workingPass]
getAllCandidates [x] workingPass = if x `elem` workingPass then [workingPass]
    else placeEverywhere x workingPass
getAllCandidates attempt@(x:y:xs) workingPass = do
    candidate <- getAllCandidates (y:xs) workingPass
    if successFullAttempt attempt candidate then return candidate
    else
        let
            withXBeforeY = listsWithXBeforeLastY x y candidate
            withYAfterX = if x `elem` candidate then
                listsWithYAfterFirstX x y candidate else []
        in
            filter (successFullAttempt attempt) withXBeforeY ++ withYAfterX

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
        minLength [] = undefined
        minLength [b] = length b
        minLength (x:xs) = min (length x) (minLength xs)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    content <- readLines fileName
    let
        input = map (map digitToInt) content
        initGuess = [head input]
        genShortestPasses candidatePasses = keepListsWithMinLength . iterate candidatePasses
        in
        mapM_ (putStrLn . map intToDigit) $ foldl genShortestPasses initGuess input
