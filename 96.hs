import qualified Data.Matrix as M
import Data.Monoid
import Data.Matrix ((!))
import Data.Maybe (fromJust)
import Data.Vector (toList)
import Data.List ((\\))
import Data.List.Split (splitWhen)
import Data.Foldable (any, Foldable, foldMap)
import Prelude hiding (any)
import System.Environment (getArgs)
import Data.Char (digitToInt)

-- patching up Data.Matrix module
type Sudoku = M.Matrix Int

mtoList :: M.Matrix a -> [a]
mtoList m = concatMap (toList . \row -> M.getRow row m) [1 .. M.nrows m]

instance Data.Foldable.Foldable M.Matrix
    where foldMap f m = mconcat $ map f (mtoList m)

-- utils
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

findSoln :: Sudoku -> Maybe Sudoku
findSoln = findSoln' . Just

findSoln' :: Maybe Sudoku -> Maybe Sudoku
findSoln' Nothing = Nothing
findSoln' (Just input) =
    case isSolved input of
    True -> Just input
    False -> case dropWhile (== Nothing) solns of
        [] -> Nothing
        (x:_) -> x
        where
            solns = map findSoln (placeDigit input)

isSolved :: Sudoku -> Bool
isSolved = not . any (0 ==)

candidates :: Sudoku -> (Int, Int) -> [Int]
candidates s (row, col)
    | s ! (row, col) /= 0    = []
    | otherwise = let
        rowList = toList $ M.getRow row s
        colList = toList $ M.getCol col s
        boxList = map (s !) $ boxIndices (row, col)
        in
        (([1 .. 9] \\ rowList) \\ colList) \\ boxList

placeDigit :: Sudoku -> [Sudoku]
placeDigit s = let
        allIndices = [(x, y) | y <- [1..M.ncols s], x <- [1..M.nrows s]]
        notPlacedYet = filter ((==) 0 . (s !)) allIndices
        f :: ((Int, Int), [Int]) -> (Int, Int) -> ((Int, Int), [Int])
        f (minIdx, cand) nextIdx = if length cand < length (candidates s nextIdx)
            then (minIdx, cand)
            else (nextIdx, candidates s nextIdx)
        startPair = (head notPlacedYet, candidates s (head notPlacedYet))
        (index, potentialDigits) = foldl f startPair (tail notPlacedYet)
    in
        map (\val -> M.setElem val index s) potentialDigits

boxIndices :: (Integral a) => (a, a) -> [(a, a)]
boxIndices (row, col) = [(x, y) | x <- ran row, y <- ran col]
    where
        startNum a = ((a - 1) `div` 3) * 3 + 1
        ran a = [startNum a .. startNum a + 2]

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
    where addDigit num d = 10*num + d

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readLines fileName
    let
        sudokusList = map (map (map digitToInt)) $
            tail $ splitWhen ((==) "Grid" . head . words) fileContent
        sudokus = map M.fromLists sudokusList
        solvedSudokus = map findSoln sudokus
        sums = map (fromDigits . take 3 . toList . M.getRow 1 . fromJust)
            solvedSudokus
        in
        print $ foldl1 (+) sums
