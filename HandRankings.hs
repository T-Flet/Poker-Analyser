---- Poker Analyser Hand Ranking Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.4 - 04-05/04/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions related to ranking sets of
--          five cards (hands).
--
--   Sections:
--       0 - Imports
--       1 - Complete Rankers
--       2 - Single HandType Rankers
--       3 - HandType Instances Calculators
--       4 - HandType Instances Counters
--       5 - General Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module HandRankings where

import DataTypes
import HandTypeCheckers
import GeneralFunctions (combinations, subsetOf, descLength)

import Data.Function (on)
import Data.List (groupBy, (\\), sortBy, sort)



---- 1 - COMPLETE RANKERS ------------------------------------------------------

    -- These rankers do not take any Suit hierarchy into account, therefore
    -- there are gaps in the ranks, which do not affect any process whatsoever.
    -- Also, these functions work under the assumption that the given hand is
    -- of the correct HandType

    -- Given two sets of cards (preferably of 5 cards each, i.e. hands) return
    -- an Ordering (by evaluating what HandType they constitute and ranking them)
cmpHands :: [Card] -> [Card] -> Ordering
cmpHands = compare `on` rankHand


    -- Given any set of 5 cards (hand), return its rank
rankHand :: [Card] -> Int
rankHand cs = rankHandType cs $ bestHandType cs


    -- Given any identified HandType and its characteristic details, return its rank
rankHandType :: [Card] -> (HandType,HandTypesField) -> Int
rankHandType cs (ht,htf) = case ht of
    RoyalFlush    -> rankRoyalFlush    cs $ toS htf
    StraightFlush -> rankStraightFlush cs $ toT htf
    FourOfAKind   -> rankFourOfAKind   cs $ toV htf
    FullHouse     -> rankFullHouse     cs $ toL htf
    Flush         -> rankFlush         cs $ toS htf
    Straight      -> rankStraight      cs $ toV htf
    ThreeOfAKind  -> rankThreeOfAKind  cs $ toV htf
    TwoPair       -> rankTwoPair       cs $ toL htf
    OnePair       -> rankOnePair       cs $ toV htf
    HighCard      -> rankHighCard      cs $ toV htf



---- 2 - SINGLE HANDTYPE RANKERS -----------------------------------------------

    -- Given a RoyalFlush, return its rank among the existing ones
    -- NOTE: There are only 4, and are all equivalent (4/4)
rankRoyalFlush :: [Card] -> Suit -> Int
rankRoyalFlush cs sui = minRank RoyalFlush


    -- Given a StraightFlush, return its rank among the existing ones
    -- NOTE: There are 32, but they depend only on the value of their highest
    -- card, therefore there are 13, but realistically only 8 (32/4)
rankStraightFlush :: [Card] -> (Suit,Value) -> Int
rankStraightFlush cs (sui,val) = minRank StraightFlush + fromEnum val


    -- Given a FourOfAKind, return its rank among the existing ones
    -- NOTE: There are 624, but they depend only on the value of the repeated
    -- card and the fifth one, therefore there are only 13*12 (624/4)
rankFourOfAKind :: [Card] -> Value -> Int
rankFourOfAKind = rankNPlet FourOfAKind


    -- Given a FullHouse, return its rank among the existing ones
    -- NOTE: There are 3744, but they depend only on the values of the triplet
    -- and the pair, therefore there are only 13*12 (3744/4 and / other combinations)
rankFullHouse :: [Card] -> [Value] -> Int
rankFullHouse cs vals = minRank FullHouse + (fromEnum three)*13 + (fromEnum pair)
    where three = head vals
          pair  = last vals


    -- Given a Flush, return its rank among the existing ones
    -- NOTE: There are 5112, but they depend only on the values of their cards
    -- therefore there are 13*12*11*10*9, from which to subtract all the
    -- Straight and Royal Flushes
rankFlush :: [Card] -> Suit -> Int
rankFlush cs sui = minRank Flush + sum (map (fromEnum . value) cs)


    -- Given a Straight, return its rank among the existing ones
    -- NOTE: There are 9180, but they depend only on the value of their highest
    -- card, therefore there are 13, but realistically only 8 (32/4)
rankStraight :: [Card] -> Value -> Int
rankStraight cs val = minRank Straight + fromEnum val


    -- Given a ThreeOfAKind, return its rank among the existing ones
    -- NOTE: There are 54912, but they depend only on the value of the repeated
    -- card and the other two's, and all the FullHouses have to be removed,
    -- therefore there are only 13*12*11
rankThreeOfAKind :: [Card] -> Value -> Int
rankThreeOfAKind = rankNPlet ThreeOfAKind


    -- Given a TwoPair, return its rank among the existing ones
    -- NOTE: There are 123552, but they depend only on the values of the first
    -- and second pair and the fifth card, excluding all FullHouses and
    -- FourOfAKinds, therefore there are only 13*12*11
rankTwoPair :: [Card] -> [Value] -> Int
rankTwoPair cs vals = minRank TwoPair + valuesBaseSum
    where valuesBaseSum = sum $ zipWith (*) (map (13^) [0..]) addenda
          addenda = reverse $ [fromEnum first, fromEnum second, fifthCard]
          fifthCard = fromEnum . head $ filter ((`notElem` vals) . value) cs
          first  = head vals
          second = last vals


    -- Given a OnePair, return its rank among the existing ones
    -- NOTE: There are 1098240, but they depend only on the value of the repeated
    -- card and the other three's, and all the FourOfAKinds, ThreeOfAKinds,
    -- FullHouses and TwoPairs have to be removed, therefore there are only 13*12*11*10
rankOnePair :: [Card] -> Value -> Int
rankOnePair = rankNPlet OnePair


    -- Given a OnePair, return its rank among the existing ones
    -- NOTE: There are 1303560, but they depend only on the value of the highest
    -- card and the sum of the others'
rankHighCard :: [Card] -> Value -> Int
rankHighCard cs val = minRank HighCard + (fromEnum val)*13 + otherCardsSum
    where otherCardsSum = sum $ map (fromEnum . value) otherCards
          otherCards = filter ((/= val) . value) cs



---- 3 - HANDTYPE INSTANCES CALCULATORS ----------------------------------------

    -- NOTE: These functions do not necessarily cater for a null input

    -- Return the number of possible RoyalFlushes which can be formed by
    -- completing the hand of the given cards and which cards are required
countRoyalFlush :: [Card] -> (Int,[[Card]])
countRoyalFlush cs
--    | null cs            = (4, map (\s-> map (\v-> Card v s) ovs) $ enumFrom Spades)
    | okVals && sameSuit = (1, [(map (\v-> Card v (suit $ head cs)) ovs) \\ cs])
    | otherwise          = (0, [])
        where okVals   = all ((`elem` ovs) . value) cs
              sameSuit = length (groupBy ((==) `on` suit) cs) == 1
              ovs = enumFrom Ten


    -- Return the number of possible StraightFlushes which can be formed by
    -- completing the hand of the given cards and which cards are required
countStraightFlush :: [Card] -> (Int,[[Card]])
countStraightFlush cs
--    | null cs            = (40, concat . map (\s-> map (map (\v->Card v s)) ovs) $ enumFrom Spades)
    | okVals && sameSuit = (length possHands, map (\\ cs) possHands)
    | otherwise          = (0, [])
        where okVals   = length possHands > 0
              sameSuit = length (suitDescGroups cs) == 1
              possHands = map (map (\v-> Card v (suit $ head cs))) $ filter ((map value cs) `subsetOf`) ovs
              ovs = (:) ((:) Ace $ enumFromTo Two Five) $ zipWith (\f t-> enumFromTo f t) (enumFromTo Two Ten) (enumFromTo Six Ace)


    -- Return the number of possible FourOfAKinds which can be formed by
    -- completing the hand of the given cards and which cards are required
countFourOfAKind :: [Card] -> (Int,[[Card]])
countFourOfAKind cs
--    | null cs   = (13, map (\v-> map (Card v) $ enumFrom Spades) $ enumFrom Two)
    | okVals    = if (length h) == 1
                    then (2, [(left h), left l])
                    else (1, [left h])
    | otherwise = (0, [])
        where okVals = (length vgcs < 3) &&
                        (if length vgcs == 2 then (length l) == 1 else True)
              left vgc = (map (Card (value $ head vgc)) $ enumFrom Spades) \\ cs
              (h,l) = (head vgcs, last vgcs)
              vgcs = valueDescGroups cs


    -- Return the number of possible FullHouses which can be formed by
    -- completing the hand of the given cards and which cards are required
--countFullHouse :: [Card] -> (Int,[[Card]])
--countFullHouse cs
----    | null cs   = (13*12, concat . map (\x y-> [[x,x,x,y,y],[y,y,y,x,x])) . ...
--    | okVals    = case length vgcs of
--                    1 -> (12, )
--                    2 -> if length h == 3
--                            then (1, )
--                            else (2, )
--    | otherwise = (0, [])
--        where okVals = (length vgcs < 3) &&
--                        (length h < 4) &&
--                        (if length vgcs == 2 then length l < 3 else True)
--              (h,l) = (head vgcs, last vgcs)
--              vgcs = valueDescGroups cs



---- 4 - HANDTYPE INSTANCES COUNTERS -------------------------------------------

    -- NOTE: This section is the "long" and "wrong" way to get to these values,
    -- but it was easy to set up as a working version.
    -- The same functions are be done through the `choose` function in the
    -- HandType Instances Calculators section.


    -- Cached result of: allHandTypesIn allCards
    -- NOTE: (sum $ map snd totHtsCounts) == (52 `choose` 5)
totHtsCounts :: [(HandType,Int)]
totHtsCounts = [
        (HighCard,1303560),
        (OnePair,1098240),
        (TwoPair,123552),
        (ThreeOfAKind,54912),
        (Straight,9180),
        (Flush,5112),
        (FullHouse,3744),
        (FourOfAKind,624),
        (StraightFlush,32),
        (RoyalFlush,4)
    ]


    -- Return the list of all HandTypes and how many "real" instances of each
    -- exist in a given set of cards , i.e. taking into account the fact that if
    -- some cards constitute more than one HandType, they should count only as
    -- the highest one
allHandTypesIn :: [Card] -> [(HandType,Int)]
allHandTypesIn cs = identifyHts . foldl countHandTypes zeroes $ handCombinations cs
    where identifyHts = zip (enumFrom HighCard)
          zeroes = replicate 10 0
          countHandTypes counts hand = concat [
                    take itsHtNum counts,
                    [(counts!!itsHtNum) + 1],
                    drop (itsHtNum + 1) counts
                ]
            where itsHtNum = fromEnum . fst . bestHandType $ hand


    -- Return how many hands have a specific HandType as their highest one
totalHt :: HandType -> Int
totalHt ht = ht `handTypeIn` allCards


    -- Return how many hands have a specific HandType as their highest one
    -- within a set of cards
handTypeIn :: HandType -> [Card] -> Int
ht `handTypeIn` cs = foldl countHandType 0 $ handCombinations cs
    where countHandType count hand
            | itsHt == ht = count + 1
            | otherwise   = count
            where itsHt = fst . bestHandType $ hand


    -- Return all possible 5-card combinations from the given cards
handCombinations :: [Card] -> [[Card]]
handCombinations = intsLToCardsL . combinations 5 . cardsToInts



---- 5 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Given an N-Plet kind of hand, return its rank among the existing ones
rankNPlet :: HandType -> [Card] -> Value -> Int
rankNPlet ht cs val = minRank ht + n*(fromEnum val)*13 + otherCardsSum
    where -- n is just a scaling factor in order to separate the highest lower
          -- specific N-Plet hands from the lowest higher ones.
          -- i.e. a pair of 2 with an Ace, King and Queen from a pair of 3 with
          -- a 2, a 4 and a 5.
          -- NOTE: In fact, the factor is only necessary for THAT specific case
          n
            | ht == OnePair = 3
            | otherwise     = 5 - length otherCards
          otherCardsSum = sum $ map (fromEnum . value) otherCards
          otherCards = filter ((/= val) . value) cs


    -- Return the rank of the lowest specified HandType in the HandType counts
    -- for all cards
minRank :: HandType -> Int
minRank = minRankIn totHtsCounts


    -- Return the rank of the lowest specified HandType in the given HandType
    -- counts
minRankIn :: [(HandType,Int)] -> HandType -> Int
minRankIn htCounts ht = sum $ map (snd . (htCounts!!)) [0..htNum - 1]
    where htNum = fromEnum ht

