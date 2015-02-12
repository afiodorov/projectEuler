import Data.Char (intToDigit, digitToInt)
import System.Environment (getArgs)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- return a list of lists with a in every possible position
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

-- generates updated passwords from an attempt
shortestPasses :: (Ord a) => [a] -> [a] -> [[a]]
shortestPasses attempt [] = [attempt]
shortestPasses [] password = [password]
shortestPasses [x] password = if x `elem` password then [password]
    else placeEverywhere x password
shortestPasses attempt@(x:y:xs) password = do
    potentialPass <- shortestPasses (y:xs) password
    if successFullAttempt attempt potentialPass then return potentialPass
    else
        let
            withXBeforeY = listsWithXBeforeLastY x y potentialPass
            withYAfterX = if x `elem` potentialPass then
                listsWithYAfterFirstX x y potentialPass else []
        in
            filter (successFullAttempt attempt) withXBeforeY ++ withYAfterX

-- checks that login attempt is succesfull for a password
successFullAttempt :: (Ord a) => [a] -> [a] -> Bool
successFullAttempt _ [] = False
successFullAttempt [] _ = True
successFullAttempt [x] xs = x `elem` xs
successFullAttempt (x:y:xs) password =
    successFullAttempt (y:xs) (tail' (dropWhile (/= x) password))
    where
        tail' [] = []
        tail' ys = tail ys

genPassesForEachPass :: (Ord a) => [[a]] -> [a] -> [[a]]
genPassesForEachPass passes attempt = passes >>= shortestPasses attempt

keepWithMinLength :: [[a]] -> [[a]]
keepWithMinLength a = filter (\b -> length b == minL) a
    where
        minL = minLength a
        minLength :: [[a]] -> Int
        minLength [] = undefined
        minLength [b] = length b
        minLength (x:xs) = min (length x) (minLength xs)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readLines fileName
    let
        attempts = map (map digitToInt) fileContent
        startingPass = [head attempts]
        genShortestPasses candidatePasses =
            keepWithMinLength . genPassesForEachPass candidatePasses
        in
        mapM_ (putStrLn . map intToDigit) $
            foldl genShortestPasses startingPass attempts
