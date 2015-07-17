---- General Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          1.5 - 16-17/07/2015
--
--      Description:
--          This package contains general functions which can be useful in many
--          different situations.
--
--   Sections:
--       0 - Imports
--       1 - Numerical Bases
--       2 - Combinations
--       3 - Sorting and Grouping Functions
--       4 - Set Related Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module GeneralFunctions where

import Data.Function (on)
import Data.List (nub, foldl')
import Data.Char (chr, ord, isDigit, isUpper, isLower)



---- 1 - NUMERICAL BASES -------------------------------------------------------

    -- Base b digits of decimal v
toBase :: Int -> Int -> [Int]
toBase b = toBase' []
    where toBase' a 0 = a
          toBase' a v = toBase' (r:a) q
            where (q,r) = v `divMod` b

    -- Decimal Int from base b digits ds
fromBase :: Int -> [Int] -> Int
fromBase b = foldl' (\n k -> n * b + k) 0


    -- Note: The following pair of functions only works for bases up to 10+27

    -- String of single Char digits (using numbers first, then lower and then
    -- capital alphabet) from list of Int digits
toAlphaDigits :: [Int] -> String
toAlphaDigits = map convert
    where convert n
            | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)

    -- Opposite of above
fromAlphaDigits :: String -> [Int]
fromAlphaDigits = map convert
    where convert c
            | isDigit c = ord c - ord '0'
            | isUpper c = ord c - ord 'A' + 10
            | isLower c = ord c - ord 'a' + 10



---- 2 - COMBINATIONS ----------------------------------------------------------

    -- Classic mathematical function
choose :: Integral a => a -> a -> a
infixl 5 `choose`
n `choose` k = fromIntegral $ product [sk+1..sn] `div` product [1..sn-sk]
    where [sn,sk] = map fromIntegral [n,k]


    -- Return all the "choose" combinations of length k from a list of length n
    -- NOTE: If the input list is sorted, so will the output list of lists
    -- NOTE: This is a direct translation of the mathematical recurrence
    -- relation of Binomial Coefficients
    -- NOTE: (length $ combinations k xs) == ((length xs) `choose` k)
    -- NOTE: If using this function leads to a non-exhaustive condition error,
    --          the cause probably is negative input values
    -- NOTE: If k > length xs the result will always be 1
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' _ _  [] = []
        combinations' n k' yys@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [yys]
          | otherwise = map (y:) nkMinus1 ++ nMinus1
            where nkMinus1 = combinations' (n-1) (k'-1) ys
                  nMinus1  = combinations' (n-1)  k'    ys


    -- Same as combinations, but with the faculty of "picking up" computation
    -- from a specific point
    -- NOTE: xs MUST be sorted for this to work
    -- NOTE: combinationsFrom k xs startComb == dropWhile (/= startComb) (combinations k xs)
    --          but much faster for large lists
    -- NOTE: startComb (as it is, without rearrangements) SHOULD be of length k
    --          and also an element of the equivalent combinations result;
    --          if not, only the beginning k elements will be considered for the
    --          process, and the list will start from the first combination with
    --          elements greater than or equal to those k ones.
combinationsFrom :: (Eq a, Ord a) => Int -> [a] -> [a] -> [[a]]
combinationsFrom k xs = combinations' (length xs) k xs
  where combinations' _ _  []         _  = []
        combinations' _ k'  yys       [] = combinations k' yys  -- Faster than: map (y:) (nkMinus1 []) ++ nMinus1 []
        combinations' n k' yys@(y:ys) ccs@(c:cs)
          | k' == 0   = [[]]
          | k' >= n   = [yys]
          | otherwise = case compare y c of
                            LT -> nMinus1 ccs
                            EQ -> map (y:) (nkMinus1 cs) ++ nMinus1 []
                            GT -> map (y:) (nkMinus1 []) ++ nMinus1 []
            where nkMinus1 = combinations' (n-1) (k'-1) ys
                  nMinus1  = combinations' (n-1)  k'    ys


    -- Cardinality of the set of xs for which any of the given conditions hold
    -- Note: This was originally supposed to be an implementation of the
    -- Inclusion-Exclusion Principle for infinite conditions over any list, but
    -- any real implementation of it would be less efficient than this
inclExclPrinc :: [a -> Bool] -> [a] -> Int
inclExclPrinc conds = foldl anyApply 0
    where anyApply count x
            | any ($x) conds = count + 1
            | otherwise      = count

    -- Alternated Signs Sum: given a list of numbers, sum them with alternated
    -- signs (the first one being +). Useful in an implementation of the
    -- Inclusion-Exclusion principle
altSignsSum :: Num a => [a] -> a
altSignsSum = snd . foldl add (1,0)
    where add (sgn,tot) v = (-sgn, tot + sgn * v)



---- 3 - SORTING AND GROUPING FUNCTIONS ----------------------------------------

    -- Descending and ascending list ordering function. Perfect for: sortBy XXX $ [[a]]
descLength, ascLength :: [a] -> [a] -> Ordering
descLength = flip ascLength
ascLength = compare `on` length


    -- Stricter version of groupBy in the sense that does not assume that the
    -- provided comparison function is an equivalence relation.
    -- Only transitivity is assumed here
    -- i.e. This function compares adjacent values, and does not take the
    -- ("wrong") shortcut that the real groupBy does
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _   []  = []
groupBy' _   [x] = [[x]]
groupBy' cmp (x:xs@(x':_))
    | cmp x x'   = (x:y):ys
    | otherwise  = [x]:r
        where r@(y:ys) = groupBy' cmp xs


    -- UnZip of lists of lists of any-(but fixed throughout)-length
    -- Eg: [[11,12,13,14],[21,22,23,24],[31,32,33,34]] ->
    --                          [[11,21,31],[12,22,32],[13,23,33],[14,24,34]]
    -- Notice: listUnZip . listUnZip == id
listUnZip :: [[a]] -> [[a]]
listUnZip []        = []
listUnZip xss@(h:_) = foldl separate (replicate (length h) []) xss
    where separate acc xs = zipWith (++) acc $ map (:[]) xs



---- 4 - SET RELATED FUNCTIONS -------------------------------------------------

    -- Check whether a set is a subset of a second set
subsetOf, notSubsetOf :: (Eq a) => [a] -> [a] -> Bool
a `subsetOf` b    = all (`elem` b)    a
a `notSubsetOf` b = any (`notElem` b) a


    -- Eliminate all supersets of any element from a list of ascending length lists
noSupersets :: Eq a => [[a]] -> [[a]]
noSupersets ascXs = noSupersets' . smallestLength $ nub ascXs
    where smallestLength [] = ([],[])
          smallestLength xxs@(x:_) = span ((== length x) . length) xxs
          noSupersets' ([],[]) = []
          noSupersets' (small,others) = (++) small . noSupersets' $ smallestLength newOthers
            where newOthers = filter (\o-> not $ any (`subsetOf` o) small) others
