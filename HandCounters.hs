---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.1 - 09-10/05/2015
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
countRoyalFlush d ocs cs = checkLeft RoyalFlush csl ph ct
    where (csl, ph, ct) = countPossHands aph d ocs cs
          aph = fromSVG (enumFrom Spades) (enumFrom Ten)


    -- Return the HandTypeCount of possible instances of StraightFlush which can
    -- be obtained by completing the set of 7 cards
countStraightFlush :: Deck -> [Card] -> [Card] -> HandTypeCount
countStraightFlush d ocs cs = checkLeft StraightFlush csl ph ct
    where (csl, ph, ct) = countPossHands aph d ocs cs
          aph = concat $ map (fromSVG (enumFrom Spades)) okvss
          okvss = take 10 . map (take 5) . tails $ Ace:(enumFrom Two)


    -- Return the HandTypeCount of possible instances of FourOfAKind which can
    -- be obtained by completing the set of 7 cards
countFourOfAKind :: Deck -> [Card] -> [Card] -> HandTypeCount
countFourOfAKind d ocs cs = checkLeft StraightFlush csl nph ct
    where nph = ph
          -- MAKE THIS RETURN NEATER CardSetS!!!!!!!
          (csl, ph, ct) = countPossHands aph d ocs cs
          aph = concat . map (\val-> fromVSG [val] (enumFrom Spades)) $ enumFrom Two



countFourOfAKindOLD :: Deck -> [Card] -> (Int,[[Card]])
countFourOfAKindOLD d cs
--    | null cs   = (13, fromVSG (enumFrom Two) (enumFrom Spades))
    | okVals    = if (length h) == 1
                    then (2, [(left h), left l])
                    else (1, [left h])
    | otherwise = (0, [])
        where okVals = (length vgcs < 3) &&
                        (if length vgcs == 2 then (length l) == 1 else True)
              left vgc = (concat $ fromVSG [value $ head vgc] (enumFrom Spades)) \\ cs
              (h,l) = (head vgcs, last vgcs)
              vgcs = valueDescGroups cs


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

    -- Conditional clause common to all count functions, checking whether enough
    -- cards are left to be drawn
checkLeft :: HandType -> Int -> [[Card]] -> [(Int,Float)] -> HandTypeCount
checkLeft ht cardsLeft possHands countTuples
    | cardsLeft > 0 = HandTypeCount ht (length possHands) (CCC possHands) countTuples
    | otherwise     = HandTypeCount ht 0 CN []


    -- Possible Hands narrowing down process common to all count functions
countPossHands :: [[Card]] -> Deck -> [Card] -> [Card] -> (Int, [[Card]], [(Int, Float)])
countPossHands allPossHands d outCs cs = (csLeft, possHands, countTuples)
    where countTuples = map (\l-> (length l, head l)) . group $ sort hProbs
          hProbs = map (handProb d . CC) possHands
          possHands = sortBy ascLength $ filter ((<= csLeft) . length) neededHands
          neededHands = map (\\cs) notOcsHands
          notOcsHands = filter (not . any (`elem` outCs)) allPossHands
          csLeft = 7 - length outCs - length cs


    -- Return the probability of drawing the given CardSet list from the given Deck
    -- EVOLVE THIS INTO USING ALL THE VALUES IN Deck AND CONSTRUCTORS OF CardSet
handProb :: (Fractional a) => Deck -> CardSet -> a
handProb d cS = 1 / fromIntegral ((cardsIn d) `choose` (cardSetLen cS))
