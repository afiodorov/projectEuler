import qualified Data.Matrix as M
import Data.Monoid
import Data.Matrix ((!))
import Data.Maybe()
import Data.Vector (toList)
import Data.List ((\\))
import Data.Foldable (any, Foldable, foldMap)
import Prelude hiding (any)

-- patching up Data.Matrix module
type Sudoku = M.Matrix Int

mtoList :: M.Matrix a -> [a]
mtoList m = concatMap (toList . \row -> M.getRow row m) [1 .. M.nrows m]

instance Data.Foldable.Foldable M.Matrix
    where foldMap f m = mconcat $ map f (mtoList m)

sudoku :: Sudoku
sudoku = M.fromLists [
    [0, 1, 2, 3, 4, 5, 6, 7, 8],
    [9, 0, 0, 3, 0, 5, 0, 0, 1],
    [0, 0, 1, 8, 0, 6, 4, 0, 0],
    [0, 0, 8, 1, 0, 2, 9, 0, 0],
    [7, 0, 0, 0, 0, 0, 0, 0, 8],
    [0, 0, 6, 7, 0, 8, 2, 0, 0],
    [0, 0, 2, 6, 0, 9, 5, 0, 0],
    [8, 0, 0, 2, 0, 3, 0, 0, 9],
    [0, 0, 5, 0, 1, 0, 3, 0, 0] ]

{-sudoku :: Sudoku-}
{-sudoku = M.fromLists [-}
    {-[0, 0, 3, 0, 2, 0, 6, 0, 0],-}
    {-[9, 0, 0, 3, 0, 5, 0, 0, 1],-}
    {-[0, 0, 1, 8, 0, 6, 4, 0, 0],-}
    {-[0, 0, 8, 1, 0, 2, 9, 0, 0],-}
    {-[7, 0, 0, 0, 0, 0, 0, 0, 8],-}
    {-[0, 0, 6, 7, 0, 8, 2, 0, 0],-}
    {-[0, 0, 2, 6, 0, 9, 5, 0, 0],-}
    {-[8, 0, 0, 2, 0, 3, 0, 0, 9],-}
    {-[0, 0, 5, 0, 1, 0, 3, 0, 0] ]-}

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
        firstIdx = head notPlacedYet
        firstCandidates = candidates s firstIdx
        f :: ((Int, Int), [Int]) -> (Int, Int) -> ((Int, Int), [Int])
        f (minIdx, cand) nextIdx = if length cand < length (candidates s nextIdx)
            then (minIdx, cand)
            else (nextIdx, candidates s nextIdx)
        (index, potentialDigits) = foldl f (firstIdx, firstCandidates) notPlacedYet
    in
        map (\val -> M.setElem val index s) potentialDigits

boxIndices :: (Integral a) => (a, a) -> [(a, a)]
boxIndices (row, col) = [(x, y) | x <- ran row, y <- ran col]
    where
        startNum a = ((a - 1) `div` 3) * 3 + 1
        ran a = [startNum a .. startNum a + 2]

main :: IO ()
main = print $ findSoln sudoku
