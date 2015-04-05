---- Poker Analyser HandType Checking and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.2 - 05-06/04/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions which test whether Card
--          lists constitute some HandType and related functions.
--
--   Sections:
--       0 - Imports
--       1 - Complete Checkers
--       2 - Single HandTypes Checkers
--       3 - General Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module HandTypeCheckers where

import GeneralFunctions (descLength, groupBy', subsetOf)
import DataTypes

import Data.List (sort, sortBy, group)
import Data.Function (on)



---- 1 - COMPLETE CHECKERS -----------------------------------------------------

    -- Return the best HandType that the given cards constitute
    -- NOTE: Same as but much faster than: head . whatIs
bestHandType :: [Card] -> (HandType,HandTypesField)
bestHandType cs
    | Just htv <- isRoyalFlush    cs = (RoyalFlush,    HS htv)
    | Just htv <- isStraightFlush cs = (StraightFlush, HT htv)
    | Just htv <- isFourOfAKind   cs = (FourOfAKind,   HV htv)
    | Just htv <- isFullHouse     cs = (FullHouse,     HL htv)
    | Just htv <- isFlush         cs = (Flush,         HS htv)
    | Just htv <- isStraight      cs = (Straight,      HV htv)
    | Just htv <- isThreeOfAKind  cs = (ThreeOfAKind,  HV htv)
    | Just htv <- isTwoPair       cs = (TwoPair,       HL htv)
    | Just htv <- isOnePair       cs = (OnePair,       HV htv)
    | Just htv <- isHighCard      cs = (HighCard,      HV htv)


    -- Return all the HandTypes that the given cards constitute
whatIs :: [Card] -> [(HandType,HandTypesField)]
whatIs cs = concat [rF, sF, fK, fH, fl, st, tK, tP, oP, hC]
    where rF = hTCard isRoyalFlush    HS RoyalFlush
          sF = hTCard isStraightFlush HT StraightFlush
          fK = hTCard isFourOfAKind   HV FourOfAKind
          fH = hTCard isFullHouse     HL FullHouse
          fl = hTCard isFlush         HS Flush
          st = hTCard isStraight      HV Straight
          tK = hTCard isThreeOfAKind  HV ThreeOfAKind
          tP = hTCard isTwoPair       HL TwoPair
          oP = hTCard isOnePair       HV OnePair
          hC = hTCard isHighCard      HV HighCard

          hTCard :: ([Card] -> Maybe a) -> (a -> HandTypesField) -> HandType -> [(HandType,HandTypesField)]
          hTCard hTChecker constructor hT = maybe [] tupler $ hTChecker cs
            where tupler x = [(hT, constructor x)]



---- 2 - SINGLE HANDTYPE CHECKERS ----------------------------------------------

    -- Return the Value of the highest card
    -- This will always be true; the Maybe is there just for consistency
isHighCard :: [Card] -> Maybe Value
isHighCard = Just . value . last . sort


    -- Return the Value of the N-plet
isOnePair, isThreeOfAKind, isFourOfAKind :: [Card] -> Maybe Value
isOnePair      = isNplet 2
isThreeOfAKind = isNplet 3
isFourOfAKind  = isNplet 4


    -- Return the Values of the N-Plets in descending order
isTwoPair, isFullHouse :: [Card] -> Maybe [Value]
isTwoPair   = is2Nplet 2 2
isFullHouse = is2Nplet 3 2


    -- Returns the value of the highest card in the Straight
isStraight :: [Card] -> Maybe Value
isStraight cs = isLenType 5 value $ inOrder cs


    -- Returns the Suit of the Flush
isFlush :: [Card] -> Maybe Suit
isFlush cs = isLenType 5 suit $ suitDescGroups cs


    -- Returns the Suit and the Value of the highest card in the StraightFlush
    -- Does not simply return a Card (which has the same fields) for pattern matching's sake
    -- NOTE: The first pattern match could have also been the real isStraight,
    --       but inOrder would have been applied twice (not as efficient)
isStraightFlush :: [Card] -> Maybe (Suit,Value)
isStraightFlush cs
    | Just val <- isStraight' , Just sui <- isFlush hocs = Just (sui, val)
    | otherwise = Nothing
        where hocs = head ocs
              isStraight' = isLenType 5 value ocs
              ocs = inOrder cs


    -- Returns the Suit of the RoyalFlush
    -- (If Suit hierarchy were implemented, it could only be Hearts)
isRoyalFlush :: [Card] -> Maybe Suit
isRoyalFlush cs = case isStraightFlush cs of
    Just (s,Ace) -> Just s
    _            -> Nothing



---- 3 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Check whether some cards constitute an N-plet (N=2 => Pair, ...)
isNplet :: Int -> [Card] -> Maybe Value
isNplet n cs = isLenType n value $ valueDescGroups cs


    -- Check whether some cards constitute two N-plets (N=3 and 2 (or 2 and 3) => FullHouse, ...)
is2Nplet :: Int -> Int -> [Card] -> Maybe [Value]
is2Nplet n m cs
    | length xg >= a && length yg >= b = if n == m
                -- If TwoPair, give the higher value first
                then Just . map value . reverse $ sort [x,y]
                else Just [value x, value y]
    | otherwise                        = Nothing
        where xg@(x:_):yg@(y:_):_ = valueDescGroups cs
              [b,a] = sort [n,m]


    -- Return a list of lists of cards grouped if of consecutive values
    -- NOTE: Caters for Aces being both lower than Twos and higher than Kings by
    -- duplicating them
inOrder :: [Card] -> [[Card]]
inOrder cs = sortBy descLength $ concat inOrder'
    where inOrder' = [map (map head) pgvgs] ++ (singleCards $ map (map tail) pgvgs)
          singleCards = map group . concat . noNull . map noNull
          noNull = filter (not . null)

          pgvgs :: [[[Card]]]
          pgvgs
            | [Ace, Two] `subsetOf` values = (init pgvgs') ++ [last pgvgs' ++ [[head aces]]] ++ tailAces
            | otherwise = pgvgs'

          values = map (value . head) vgs
          tailAces
            | length aces > 1 = [[tail aces]]
            | otherwise       = []
          aces = head vgs

          pgvgs' :: [[[Card]]]
          pgvgs' = groupBy' (isPred `on` (value . head)) vgs
          vgs = valueGroups cs
          isPred v1 v2 = case v1 of
                Two -> False
                _   -> v2 == pred v1


    -- Given a list of lists of cards, return the suit or value of the first
    -- element in the first list in it if the given list's length is greater
    -- than or equal to n
isLenType :: Int -> (Card -> a) -> [[Card]] -> Maybe a
isLenType n suitOrValue cs
    | length hcs >= n = Just . suitOrValue $ head hcs
    | otherwise       = Nothing
        where hcs = head cs

