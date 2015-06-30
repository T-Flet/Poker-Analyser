---- Poker Analyser HandType Checking and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.7 - 29/05/2015
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

import Data.List (sort, sortBy, maximumBy, group, (\\))
import Data.Function (on)



---- 1 - COMPLETE CHECKERS -----------------------------------------------------

    -- Return the best HandType that the given cards constitute
    -- and its carachteristic fields and its actual 5 cards
    -- NOTE: Same as but much faster than: head . whatIs
bestHandType :: [Card] -> Hand
bestHandType cs
    | Just (htv,ncs) <- isRoyalFlush    cs = Hand RoyalFlush    (HS htv) 0 ncs
    | Just (htv,ncs) <- isStraightFlush cs = Hand StraightFlush (HT htv) 0 ncs
    | Just (htv,ncs) <- isFourOfAKind   cs = Hand FourOfAKind   (HV htv) 0 ncs
    | Just (htv,ncs) <- isFullHouse     cs = Hand FullHouse     (HL htv) 0 ncs
    | Just (htv,ncs) <- isFlush         cs = Hand Flush         (HS htv) 0 ncs
    | Just (htv,ncs) <- isStraight      cs = Hand Straight      (HV htv) 0 ncs
    | Just (htv,ncs) <- isThreeOfAKind  cs = Hand ThreeOfAKind  (HV htv) 0 ncs
    | Just (htv,ncs) <- isTwoPair       cs = Hand TwoPair       (HL htv) 0 ncs
    | Just (htv,ncs) <- isOnePair       cs = Hand OnePair       (HV htv) 0 ncs
    | Just (htv,ncs) <- isHighCard      cs = Hand HighCard      (HV htv) 0 ncs


    -- Return all the HandTypes that the given cards constitute
    -- and their carachteristic fields and their actual 5 cards
whatIs :: [Card] -> [Hand]
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

          hTCard :: ([Card] -> Maybe (a,[Card])) -> (a -> HandTypesField) -> HandType -> [Hand]
          hTCard hTChecker constructor hT = maybe [] hander $ hTChecker cs
            where hander (x,ncs) = [Hand hT (constructor x) 0 ncs]



---- 2 - SINGLE HANDTYPE CHECKERS ----------------------------------------------

    -- Returns the Suit of the RoyalFlush and its constituting cards
    -- (If Suit hierarchy were implemented, it could only be Hearts)
isRoyalFlush :: [Card] -> Maybe (Suit,[Card])
isRoyalFlush cs = case isStraightFlush cs of
    Just ((s,Ace),ncs) -> Just (s, ncs)
    _                  -> Nothing


    -- Returns the Suit and the Value of the highest card in the StraightFlush and its constituting cards
    -- Does not simply return a Card (which has the same fields) for pattern matching's sake
isStraightFlush :: [Card] -> Maybe ((Suit,Value),[Card])
isStraightFlush cs
    | null flushes = Nothing
    | null straightFlushes = Nothing
    | otherwise = Just $ maximumBy (compare `on` (snd . fst)) straightFlushes
        where straightFlushes = foldr getStrFl [] flushes
              getStrFl fl sfls = case isStraight fl of
                Just (v,sfcs) -> ((suit $ head fl, v), sfcs):sfls
                Nothing       -> sfls
              flushes = filter ((>=5) . length) $ suitDescGroups cs


    -- Return the Value of the N-plet and its 5 constituting cards
isFourOfAKind, isThreeOfAKind, isOnePair :: [Card] -> Maybe (Value,[Card])
isFourOfAKind  = isNplet 4
isThreeOfAKind = isNplet 3
isOnePair      = isNplet 2


    -- Return the Values of the N-Plets in descending order and their 5 constituting cards
isFullHouse, isTwoPair :: [Card] -> Maybe ([Value],[Card])
isFullHouse = is2Nplet 3 2
isTwoPair   = is2Nplet 2 2


    -- Returns the Suit of the Flush and its constituting cards
isFlush :: [Card] -> Maybe (Suit,[Card])
isFlush cs = isLenType 5 suit $ suitDescGroups cs


    -- Returns the value of the highest card in the Straight and its constituting cards
isStraight :: [Card] -> Maybe (Value,[Card])
isStraight cs = isLenType 5 value inOrderAndOk
    where inOrderAndOk = sortBy ((invCompare) `on` (value . head)) . filter ((>=5) . length) $ inOrder cs
            -- More efficient than a reverse after the sort
          invCompare a b = case compare a b of
            LT -> GT
            EQ -> EQ
            GT -> LT


    -- Return the Value of the highest card and the 5 highest cards
    -- This will always be true; the Maybe is there just for consistency
isHighCard :: [Card] -> Maybe (Value,[Card])
isHighCard = Just . vAndFive . sort
    where vAndFive scs = (value $ last scs, take 5 scs)



---- 3 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Check whether some cards constitute an N-plet (N=2 => Pair, ...)
    -- and return the value and the 5 constituting cards
isNplet :: Int -> [Card] -> Maybe (Value,[Card])
isNplet n cs = isLenType n value $ valueDescGroups cs


    -- Check whether some cards constitute two N-plets (N=3 and 2 (or 2 and 3) => FullHouse, ...)
    -- and return their values and the constituting cards
is2Nplet :: Int -> Int -> [Card] -> Maybe ([Value],[Card])
is2Nplet n m cs
    | length gs >= 2 && length xg >= a && length yg >= b = if n == m
                -- If TwoPair, give the higher value first
                then Just (xys, xygs)
                else Just ([value x, value y], xygs)
    | otherwise                        = Nothing
        where xygs
                | length xygs' < 5 = xygs' ++ (take (5 - length xygs') $ (concat gs) \\ xygs')
                | otherwise        = xygs'
              xygs' = concat [take a xg, take b yg]
              xys = map value . reverse $ sort [x,y]
              xg@(x:_):yg@(y:_):zgs = gs
              -- The reason why the above pattern is not matched directly
              -- below is because the gs length check needs to happen before it
              gs = valueDescGroups cs
              [b,a] = sort [n,m]


    -- Given a list of lists of cards, return the suit or value of the first
    -- element in the first list in it if the Card list's length is at least n.
    -- Also return the first 5 cards
    -- Note: it seems like a weird task, but it is common to many check functions
isLenType :: Int -> (Card -> a) -> [[Card]] -> Maybe (a,[Card])
isLenType n suitOrValue css
    | null css        = Nothing
    | length hcs >= n = Just (suitOrValue $ head hcs, take 5 $ concat css)
    | otherwise       = Nothing
        where hcs = head css


    -- Return a list of lists of cards grouped if of consecutive values
    -- Single lists are in descending value order
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

            -- Check whether v2 is the predecessor of v1 (YES, this way round)
          isPred v1 v2 = case v1 of
                Two -> False
                _   -> v2 == pred v1


