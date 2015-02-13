import Data.Char (intToDigit, digitToInt)
import System.Environment (getArgs)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- return a list of lists with a in every possible position
placeEverywhere :: a -> [a] -> [[a]]
placeEverywhere a [] = [[a]]
placeEverywhere a (x:xs) = (a:x:xs) : map ((:) x) (placeEverywhere a xs)

listsWithXBeforeLastY :: (Ord a) => a -> a -> [a] -> [[a]]
listsWithXBeforeLastY x y = map reverse . listsWithYAfterFirstX y x . reverse

listsWithYAfterFirstX :: (Ord a) => a -> a -> [a] -> [[a]]
listsWithYAfterFirstX x y list  = let
    (beforeX, afterX) = break (x ==) list in
    map ((beforeX ++ [x]) ++) $ placeEverywhere y (tail afterX)

-- generates updated passwords from an attempt
shortestPasses :: (Ord a) => [a] -> [a] -> [[a]]
shortestPasses attempt [] = [attempt]
shortestPasses [] password = [password]
shortestPasses [x] password = if x `elem` password then [password]
    else placeEverywhere x password
shortestPasses attempt@(x:y:xs) password = do
    potentialPass <- shortestPasses (y:xs) password
    if isSuccessFullAttempt attempt potentialPass then return potentialPass
    else
        let
            withXBeforeY = listsWithXBeforeLastY x y potentialPass
            withYAfterX = if x `elem` potentialPass then
                listsWithYAfterFirstX x y potentialPass else []
        in
            filter (isSuccessFullAttempt attempt) withXBeforeY ++ withYAfterX

-- checks that login attempt is successfull for a password
isSuccessFullAttempt :: (Ord a) => [a] -> [a] -> Bool
isSuccessFullAttempt = isSubsequence

isSubsequence :: (Ord a) => [a] -> [a] -> Bool
isSubsequence _ [] = False
isSubsequence [] _ = True
isSubsequence [x] xs = x `elem` xs
isSubsequence (x:y:xs) sequen =
    isSubsequence (y:xs) (tail' (dropWhile (/= x) sequen))
    where
        tail' [] = []
        tail' ys = tail ys

keepWithMinLength :: [[a]] -> [[a]]
keepWithMinLength xs = filter ((==) (minimum (map length xs)) . length) xs

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readLines fileName
    let
        attempts = map (map digitToInt) fileContent
        startingPasses = [head attempts]
        genShortestPasses passes attempt =
            keepWithMinLength (passes >>= shortestPasses attempt)
        in
        mapM_ (putStrLn . map intToDigit) $
            foldl genShortestPasses startingPasses attempts
