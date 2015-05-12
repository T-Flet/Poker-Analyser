---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.3 - 12/05/2015
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
import GeneralFunctions (choose, combinations, ascLength)

import Data.List (tails, group, sort, sortBy, (\\))
import Data.Sequence (replicateM)
import Data.Foldable (toList)



---- 1 - HANDTYPE INSTANCES CALCULATORS ----------------------------------------

    -- Each of the following functions returns the HandTypeCount of possible
    -- instances of a specific HandType which can be obtained by completing a
    -- set of 7 cards. Their input is:
    -- Deck -> [Card] -> [Card] -> HandTypeCount
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


countRoyalFlush = countPossHands RoyalFlush aphs
    where aphs = fromSVG allSuits (enumFrom Ten)


countStraightFlush = countPossHands StraightFlush aphs
    where aphs = concat $ map (fromSVG allSuits) apvs
          apvs = take 10 . map (take 5) . tails $ Ace:allValues


countFourOfAKind = countPossHands FourOfAKind aphs
    where aphs = concat $ map (\val-> fromVSG [val] allSuits) allValues


countFullHouse = countPossHands FullHouse aphs
    where aphs = [makeCs [v3,v3,v3] ss3 ++ makeCs [v2,v2] ss2 | (ss3,ss2) <- apsss, (v3,v2) <- apvs]
          apvs = [(v3,v2) | v3 <- allValues, v2 <- allValues, v3 /= v2]
          apsss = [(ss3,ss2) | ss3 <- combinations 3 allSuits, ss2 <- combinations 2 allSuits]


countFlush = countPossHands Flush aphs
    where aphs = [fromSV [s] vs | vs <- combinations 5 allValues, s <- allSuits]


countStraight = countPossHands Straight aphs
    where aphs = [makeCs vs ss | vs <- apvs, ss <- map toList $ replicateM 5 allSuits]
          apvs = take 10 . map (take 5) . tails $ Ace:allValues


countThreeOfAKind = countPossHands ThreeOfAKind aphs
    where aphs = [makeCs [v,v,v] ss | v <- allValues, ss <- apss]
          apss = combinations 3 allSuits


countTwoPair = countPossHands TwoPair aphs
    where aphs = [fromVS [v1] ss1 ++ fromVS [v2] ss2 | [v1,v2] <- apvs, ss1 <- apss, ss2 <- apss]
          apvs = combinations 2 allValues
          apss = combinations 2 allSuits


countOnePair = countPossHands OnePair aphs
    where aphs = [fromVS [v] ss | v <- allValues, ss <- combinations 2 allSuits]


countHighCard = countPossHands HighCard aphs
    where aphs = group allCards



---- 2 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Possible Hands narrowing down process common to all count functions
countPossHands :: HandType -> [[Card]] -> Deck -> [Card] -> [Card] -> HandTypeCount
countPossHands ht allPossHands d outCs cs
    | csLeft > 0 = HandTypeCount ht possHands countTuples
    | otherwise  = HandTypeCount ht [] []
        where countTuples = map (\l-> (length l, head l)) $ group hProbs
              hProbs = map (handProb d) possHands
              possHands = sortBy ascLength $ filter ((<= csLeft) . length) neededHands
              neededHands = filter (not . null) $ map (\\cs) notOcsHands
              notOcsHands = filter (not . any (`elem` outCs)) allPossHands
              csLeft = 7 - length outCs - length cs


    -- Return the probability of drawing the given CardSet list from the given Deck
    -- EVOLVE THIS INTO USING ALL THE VALUES IN Deck AND CONSTRUCTORS OF CardSet
handProb :: (Fractional a) => Deck -> [Card] -> a
handProb d css = 1 / fromIntegral ((cardsIn d) `choose` (length css))
