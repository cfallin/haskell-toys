-- Sudoku solver in Haskell
-- Chris Fallin <cfallin@c1f.net>, 2013-12-14

import Data.List
import Data.Maybe

data Cell =
      Known Int
    | Possible [Int]
    | Impossible

-- A board is kept as a flattened one-dimensional array, though the original
-- problem is in two dimensions.
type Board = [Cell]

-- neighbors: given a cell index i, return the list of all cells that are
-- "neighbors", where a neighbor is one that can conflict with the original
-- cell (i.e. is in the same row, column, or 3x3 block).
neighbors :: Int -> [Int]
neighbors i =
    let r = i `div` 9
        c = i `mod` 9
        nr = r - (r `mod` 3)
        nc = c - (c `mod` 3) in
    nub $                                                   -- unique cell indices...
    filter (\x -> x /= i) $                                 -- not the original cell...
        map ( \(r',c') -> 9*r' + c' )
          ([ (r,c') | c' <- [0..8] ] ++                     -- ... in the same row
           [ (r',c) | r' <- [0..8] ] ++                     -- ... or the same column
           [ (nr+r', nc+c') | r' <- [0..2], c' <- [0..2] ]) -- ... or the same 3x3 block

-- neighborVals: return the list of all values that are known in all neighbors.
neighborVals :: Board -> Int -> [Int]
neighborVals b i = nub (concat (map vals (neighbors i)))
    where vals c = case (b !! c) of
                    Impossible -> []
                    Possible l -> []
                    Known x    -> [x]

-- possibleVals: return the list of all values that are *not* in any neighbor.
possibleVals :: Board -> Int -> [Int]
possibleVals b i = [1..9] \\ (neighborVals b i)

-- resolve: for each cell with multiple possible values, try to resolve to a
-- single possible value based on neighbor cells.
resolve :: Board -> Board
resolve b = map resolveCell (zip [0..] b)
    where
        -- resolveCell cell_index, cell_val: update cell based on neighbors.
        -- impossible cells remain impossible; known cells remain known
        -- unless they became impossible; cells with multiple possibilities
        -- take on the newly computed possibility list.
        resolveCell (_, Impossible) = Impossible
        resolveCell (i, Known val) =
            let nVals = possibleVals b i in
            if length nVals == 0 then Impossible else Known val
        resolveCell (i, Possible _) =
            let vals = possibleVals b i in
            case length vals of
                0 -> Impossible
                1 -> Known (head vals)
                _ -> Possible vals

-- branch: return a list of boards with one less unknown cell by picking the
-- first cell with multiple possibilities and branching over all possibilities.
branch :: Board -> [Board]
branch b =
    case (findPossible b) of
        Just i  -> splitBoard b i
        Nothing -> [b]
    where
        -- findPossible: finds the index of the first multiple-possibility cell.
        findPossible b = findIndex (\x -> case x of Possible _ -> True; _ -> False) b
        -- splitBoard: returns a list of boards with the cell at the given
        -- index as the split point. If the cell is known, just one board;
        -- impossible, zero If the cell has N possibilities, return N boards,
        -- each with the cell Known as that value.
        splitBoard b i =
            case (b !! i) of
                Possible l -> [replaceCell b i (Known val) | val <- l]
                Known _    -> [b]
                Impossible -> []
        -- replaceCell: return a modified board with cell i set to `val`.
        replaceCell b i val =
            map (\(ci, c) -> if ci == i then val else c) (zip [0..] b)

-- isSolved: is the board solved?
isSolved :: Board -> Bool
isSolved b = all (\c -> case c of Known _ -> True; _ -> False) b

-- isImpossible: is the board impossible?
isImpossible :: Board -> Bool
isImpossible b = any (\c -> case c of Impossible -> True; _ -> False) b

-- solve: solve a board. returns Nothing if impossible or Just sol for solution
-- `sol`.
solve :: Board -> Maybe Board
solve b =
    -- first determine as much as possible based on constraints. The board may
    -- be resolved to impossible, or if not, then anything that can be known
    -- will be known and the rest of the cells have minimal possibility sets.
    let b' = resolve b in
    if isImpossible b then Nothing else
    if isSolved b then Just b else
    -- solve each sub-board branched from this board and take the first with
    -- a valid solution.
    case (find isJust $ map solve $ branch b') of
        Just (Just b) -> Just b
        Nothing -> Nothing

{- ------------------------------------- -}
{- -------------   test   -------------- -}
{- ------------------------------------- -}

sampleRaw :: [Int]
sampleRaw = [
             0, 6, 0,   1, 0, 4,   0, 5, 0,
             0, 0, 8,   3, 0, 5,   6, 0, 0,
             2, 0, 0,   0, 0, 0,   0, 0, 1,
            
             8, 0, 0,   4, 0, 7,   0, 0, 6,
             0, 0, 6,   0, 0, 0,   3, 0, 0,
             7, 0, 0,   9, 0, 1,   0, 0, 4,
            
             5, 0, 0,   0, 0, 0,   0, 0, 2,
             0, 0, 7,   2, 0, 6,   9, 0, 0,
             0, 4, 0,   5, 0, 8,   0, 7, 0
            ]
sample = map convCell sampleRaw
    where convCell 0 = Possible []
          convCell i = Known i

instance Show Cell where
    show Impossible   = "Impossible"
    show (Possible l) = "Possible[" ++ (intercalate "," (map show l)) ++ "]"
    show (Known i)    = show i

showBoard :: Board -> String
showBoard [] = ""
showBoard b = (show $ take 9 b) ++ "\n" ++ (showBoard $ drop 9 b)

showMaybeBoard :: Maybe Board -> String
showMaybeBoard (Just b) = showBoard b
showMaybeBoard Nothing  = "no solution"

main = do
    putStrLn $ "board is:\n" ++ (showMaybeBoard $ (solve sample))
