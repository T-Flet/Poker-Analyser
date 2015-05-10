---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.2 - 09-10/05/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions related to counting possible
--          Hands from given conditions.
--
--   Sections:
--       0 - Imports
--       1 - HandType Instances Counters
--       2 - General Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module HandCounters where

import DataTypes
import GeneralFunctions (choose, ascLength)

import Data.List (tails, group, sort, sortBy, (\\))



---- 1 - HANDTYPE INSTANCES CALCULATORS ----------------------------------------

    -- Input for the following functions:
    -- the current Deck, the cards which should not be considered (like the
    -- player's if working just on the table) and the cards in question.


    -- Apply all HandTypes' Instances Calculators to the given cards
--countHandTypes :: Deck -> [Card] -> [Card] -> [HandTypeCount]
--countHandTypes d ocs cs = map (\f-> f d ocs cs) countFunctions
--    where countFunctions = [countRoyalFlush, \
--                          \ countStraightFlush, \
--                          \ countFourOfAKind, \
--                          \ countFullHouse, \
--                          \ countFlush, \
--                          \ countStraight, \
--                          \ countThreeOfAKind, \
--                          \ countTwoPair, \
--                          \ countOnePair, \
--                          \ countHighCard]


    -- Return the HandTypeCount of possible instances of RoyalFlush which can be
    -- obtained by completing the set of 7 cards
countRoyalFlush :: Deck -> [Card] -> [Card] -> HandTypeCount
countRoyalFlush = countPossHands RoyalFlush aph
    where aph = fromSVG (enumFrom Spades) (enumFrom Ten)


    -- Return the HandTypeCount of possible instances of StraightFlush which can
    -- be obtained by completing the set of 7 cards
countStraightFlush :: Deck -> [Card] -> [Card] -> HandTypeCount
countStraightFlush = countPossHands StraightFlush aph
    where aph = concat $ map (fromSVG (enumFrom Spades)) okvss
          okvss = take 10 . map (take 5) . tails $ Ace:(enumFrom Two)


    -- Return the HandTypeCount of possible instances of FourOfAKind which can
    -- be obtained by completing the set of 7 cards
countFourOfAKind :: Deck -> [Card] -> [Card] -> HandTypeCount
countFourOfAKind = countPossHands FourOfAKind aph
    where aph = concat . map (\val-> fromVSG [val] (enumFrom Spades)) $ enumFrom Two


    -- Return the HandTypeCount of possible instances of FullHouse which can
    -- be obtained by completing the set of 7 cards
--countFullHouse :: Deck -> [Card] -> [Card] -> HandTypeCount
--countFullHouse = countPossHands FourOfAKind aph
--    where aph = [ | v3 <- enumFrom 2, v2 <- enumFrom 2, v3 /= v2]



    -- Return the number of possible FullHouses which can be formed by
    -- completing the hand of the given cards and which cards are required
--countFullHouse :: Deck -> [Card] -> (Int,[[Card]])
--countFullHouse d cs
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



---- 2 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Possible Hands narrowing down process common to all count functions
countPossHands :: HandType -> [[Card]] -> Deck -> [Card] -> [Card] -> HandTypeCount
countPossHands ht allPossHands d outCs cs
    | csLeft > 0 = HandTypeCount ht (length possHands) possHands countTuples
    | otherwise  = HandTypeCount ht 0 [] []
        where countTuples = map (\l-> (length l, head l)) $ group hProbs
              hProbs = map (handProb d) possHands
              possHands = sortBy ascLength $ filter ((<= csLeft) . length) neededHands
              neededHands = map (\\cs) notOcsHands
              notOcsHands = filter (not . any (`elem` outCs)) allPossHands
              csLeft = 7 - length outCs - length cs


    -- Return the probability of drawing the given CardSet list from the given Deck
    -- EVOLVE THIS INTO USING ALL THE VALUES IN Deck AND CONSTRUCTORS OF CardSet
handProb :: (Fractional a) => Deck -> [Card] -> a
handProb d css = 1 / fromIntegral ((cardsIn d) `choose` (length css))
