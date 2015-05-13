---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.4 - 12-13/05/2015
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

import Data.List (tails, group, sort, sortBy, delete, (\\))
import Data.Sequence (replicateM)
import Data.Foldable (toList)



---- 1 - HANDTYPE INSTANCES CALCULATORS ----------------------------------------

    -- Each of the following functions returns the HandTypeCount of possible
    -- instances of a specific HandType which can be obtained by completing a
    -- set of 7 cards. Their input is:

    -- Deck -> [Card] -> [Card] -> HandTypeCount
    -- the current Deck, the cards which should not be considered (like the
    -- player's if working just on the table) and the cards in question.

    -- Note: each function avoids counting instances of their HandType which
    -- qualify as a higher one as well.


    -- Apply all HandTypes' Instances Calculators to the given cards
countHandTypes :: Deck -> [Card] -> [Card] -> [HandTypeCount]
countHandTypes d ocs cs = map (\f-> f d ocs cs) countFunctions
    where countFunctions = [countRoyalFlush,
                            countStraightFlush,
                            countFourOfAKind,
                            countFullHouse,
                            countFlush,
                            countStraight,
                            countThreeOfAKind,
                            countTwoPair,
                            countOnePair,
                            countHighCard]


countRoyalFlush = countPossHands RoyalFlush aphs
    where aphs = fromSVG allSuits (enumFrom Ten)


countStraightFlush d ocs cs = countPossHands StraightFlush aphs d ocs cs
    where aphs = filter ((`notElem` cs) . succ . last) phs
            -- Remove the Straights which, if present, are overshadowed by another
            -- greater by one (the Card just above them is in cs)
            -- Note: no risk of error on succ because only 9 taken below
          phs = concat $ map (fromSVG allSuits) apvs
            -- Taking 9 and not 10 prevents RoyalFlushes
          apvs = take 9 . map (take 5) . tails $ Ace:allValues


countFourOfAKind = countPossHands FourOfAKind aphs
    where aphs = concat $ map (\val-> fromVSG [val] allSuits) allValues


countFullHouse d ocs cs = countPossHands FullHouse aphs d ocs cs
    where aphs = [makeCs [v3,v3,v3] ss3 ++ makeCs [v2,v2] ss2 | (ss3,ss2) <- apsss, (v3,v2) <- apvs, nF v3 ss3 v2 ss2]
            -- Ensuring v3 /= v2 prevents FourOfAKinds
          apvs = [(v3,v2) | v3 <- allValues, v2 <- allValues, v3 /= v2]
          apsss = [(ss3,ss2) | ss3 <- combinations 3 allSuits, ss2 <- combinations 2 allSuits]
          nF v3 ss3 v2 ss2 = noFourOfAKinds3 v3 ss3 && noFourOfAKinds2 v2 ss2
          noFourOfAKinds3 v ss = Card v (head (allSuits \\ ss)) `notElem` cs
            -- This one has a not-all-elem instead of an intuitive any-notElem
            -- because if there are 2 ThreeOfAKinds it is still a FullHouse
          noFourOfAKinds2 v ss = not . all (`elem` cs) $ makeCs (repeat v) (allSuits \\ ss)


countFlush = countPossHands Flush aphs
    where aphs = [fromSV [s] vs | vs <- apvs, s <- allSuits]
            -- Remove all FullHouses and FourOfAKinds
          apvs = filter ((>2) . length . group . sort) pvs
          pvs = combinations 5 allValues \\ npvs
            -- Remove all Straights (has to be in this order to do so efficiently)
          npvs = (enumFromTo Two Five ++ [Ace]) : npvs'
          npvs' = take 9 . map (take 5) . tails $ allValues


countStraight = countPossHands Straight aphs
    where aphs = [makeCs vs ss | vs <- apvs, ss <- apss]
          apvs = take 10 . map (take 5) . tails $ Ace:allValues
          apss = pss \\ npss
            -- Remove all StraightFlushes (and Flushes)
          npss = map (replicate 5) $ enumFrom Spades
          pss = map toList $ replicateM 5 allSuits



-- !!!!!!!!!!!!
-- CHECKED ABOVE HERE TO REMOVE COUNTING OF BETTER HandTypes INSTANCES
-- BELOW STILL TO BE CLEARED OF THEM
-- !!!!!!!!!!!!


countThreeOfAKind d ocs cs = countPossHands ThreeOfAKind aphs d ocs cs
    where aphs = [makeCs [v,v,v] (delete s allSuits) | v <- allValues, s <- allSuits, noFourOfAKinds v s]
          noFourOfAKinds v s = Card v s `notElem` cs


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
