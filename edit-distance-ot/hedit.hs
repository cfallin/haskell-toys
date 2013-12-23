module Hedit where

import Data.List
import qualified Data.Map

-- a document is just a sequence of characters
type Document = [Char]

data Edit = NoEdit
          | Add Int Char -- offset, character to add
          | Del Int      -- offset

instance Show Edit where
    show e =
        case e of
            Add offset char -> "ADD offset " ++ show offset ++ " char (" ++ show char ++ ")"
            Del offset      -> "DEL offset " ++ show offset
            NoEdit          -> "NONE"

applyEdit :: Document -> Edit -> Document
applyEdit d e =
    case e of
        NoEdit          -> d
        Add offset char -> (take offset d) ++ [char] ++ (drop offset d)
        Del offset      -> (take offset d) ++ (drop (offset + 1) d)

applyEdits :: Document -> [Edit] -> Document
applyEdits d [] = d
applyEdits d (e:es) =
    applyEdits (applyEdit d e) es

-- rebaseEdit: given two edits e1 and e2, both applicable to the same base
-- document, return a new edit e2' which applies to the modified document after
-- e1 has been applied. In other words, rebase e2 to apply after e1.
rebaseEdit :: Edit -> Edit -> Edit
rebaseEdit e1 e2 =
    let offsetMap =
            case e1 of
                NoEdit          -> [0 .. ]
                Add offset char -> [0 .. (offset - 1)] ++ [(offset + 1) .. ]
                Del offset      -> [0 .. (offset - 1)] ++ [offset] ++ [offset .. ]
    in
        case e2 of
            Add offset char -> Add (offsetMap !! offset) char
            Del offset      -> Del (offsetMap !! offset)
            NoEdit          -> NoEdit

-- rebaseEditN: rebase a single edit over a linear series of edits originating
-- from the same base document.
rebaseEditN :: [Edit] -> Edit -> Edit
rebaseEditN [] e = e
rebaseEditN (x:xs) e = rebaseEditN xs (rebaseEdit x e)

-- rebaseEdits: given two linear sets of edits, each of which originated at a
-- single original document, rebase the latter set of edits onto the end of the
-- first set of edits and return the modified edits.
rebaseEdits :: [Edit] -> [Edit] -> [Edit]
rebaseEdits es [] = es
rebaseEdits [] es = es
rebaseEdits es xs = map (rebaseEditN es) xs

-- shiftEdit: shift an edit left or right by a fixed amount.
shiftEdit :: Int -> Edit -> Edit
shiftEdit shift e =
    case e of
        NoEdit          -> NoEdit
        Add offset char -> Add (offset + shift) char
        Del offset      -> Del (offset + shift)

-- diffDocs: take the diff from d1 to d2, giving a list of edits.
--
-- This is the memoization wrapper; the real logic is in diffDocs' below.
diffDocs :: Document -> Document -> [Edit]
diffDocs d1 d2 =
    lookup d1 d2

    where
        suffixes l = map (flip drop l) [0..length l]
        d1Suffix = suffixes d1; d2Suffix = suffixes d2
        table = Data.Map.fromList
                        [ ( (a, b), diffDocs' a b lookup ) |
                            a <- d1Suffix, b <- d2Suffix ]

        lookup d1 d2 = case Data.Map.lookup (d1, d2) table of
            Nothing -> []
            Just x -> x

diffDocs' :: Document -> Document -> (Document -> Document -> [Edit]) -> [Edit]

-- base cases: one or both docs empty
diffDocs' [] [] r = []
diffDocs' d1 [] r =
    map (\c -> (Del 0)) d1
diffDocs' [] d2 r =
    map (\(i, char) -> Add i char) $ zip [0 .. ] d2

-- general case: compare first char and recurse
diffDocs' (d1:d1s) (d2:d2s) r =
    let d1all = [d1] ++ d1s
        d2all = [d2] ++ d2s in
    -- We pick one of up to three possibilities:
    --   del option: emit a 'del char' for first char of d1;
    --     recurse on the rest of d1 and all of d2.
    --   add option: emit an 'add char' for the first char of d2;
    --     recurse on all of d1 and the rest of d2, shifting offsets
    --     right by one.
    --   if first chars match, equal option: emit no edits,
    --     recurse on the rest of d1 and the rest of d2,
    --     shifting offsets right by one.
    let options =
            [ [Del 0]    ++ (r d1s d2all),
              [Add 0 d2] ++ (map (shiftEdit 1) $ (r d1all d2s)) ] ++
            if d1 == d2 then
                [map (shiftEdit 1) $ r d1s d2s]
            else []
    in
    let lengths = map length options in
    case find (\l -> length l == (minimum lengths)) options of
        Just option -> option
        Nothing     -> [] -- should never happen
