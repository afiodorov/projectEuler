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
import Control.Monad (msum)

type Sudoku = M.Matrix Int

mtoList :: M.Matrix a -> [a]
mtoList m = concatMap (toList . \row -> M.getRow row m) [1 .. M.nrows m]

instance Data.Foldable.Foldable M.Matrix
    where foldMap f m = mconcat $ map f (mtoList m)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
    where addDigit num d = 10 * num + d

findSoln :: Sudoku -> Maybe Sudoku
findSoln input
    | (isSolved input) == True = Just input
    | otherwise                = msum $ map findSoln (placeDigit input)

isSolved :: Sudoku -> Bool
isSolved = not . any (0 ==)

placeDigit :: Sudoku -> [Sudoku]
placeDigit m = map (\val -> M.setElem val sqWithLeastGuesses m) guesses
    where
        allSquares = [(x, y) | y <- [1..M.ncols m], x <- [1..M.nrows m]]
        emptySquares = filter ((==) 0 . (m !)) allSquares
        f (square, guesses') nextSquare =
            case length guesses' < length nextGuesses of
                True -> (square, guesses')
                False -> (nextSquare, nextGuesses)
                where
                    nextGuesses = candidateDigits m nextSquare
        startPair = (head emptySquares, candidateDigits m (head emptySquares))
        (sqWithLeastGuesses, guesses) = foldl f startPair (tail emptySquares)

candidateDigits :: Sudoku -> (Int, Int) -> [Int]
candidateDigits s (row, col)
    | s ! (row, col) /= 0  = undefined
    | otherwise = (([1 .. 9] \\ rowDigits) \\ colDigits) \\ boxDigits
    where
        rowDigits = toList $ M.getRow row s
        colDigits = toList $ M.getCol col s
        boxDigits = map (s !) $ boxIndices (row, col)

boxIndices :: (Integral a) => (a, a) -> [(a, a)]
boxIndices (row, col) = [(x, y) | x <- range row, y <- range col]
    where
        startNum a = ((a - 1) `div` 3) * 3 + 1
        range a = [startNum a .. startNum a + 2]

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readLines fileName
    let
        sudokusAsLists = map (map (map digitToInt)) $
            tail $ splitWhen ((==) "Grid" . head . words) fileContent
        sudokus = map M.fromLists sudokusAsLists
        solvedSudokus = map findSoln sudokus
        firstDigits = map (fromDigits . take 3 . toList . M.getRow 1 . fromJust)
            solvedSudokus
        in
        print $ foldl1 (+) firstDigits
