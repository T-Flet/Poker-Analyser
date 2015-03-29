---- Poker Analyser Probability Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.1 - 20-21/03/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions related to probability
--          calculations.
--
--   Sections:
--       0 - Imports
--       1 - Mathematical Functions
--       2 - Probability -> Hand Functions
--       3 - Hand Specific Probability Functions
--       4 - Quality Related Functions
--       5 - HandType instances counters
--



---- 0 - IMPORTS ---------------------------------------------------------------

module Probabilities where

import DataTypes
import HandTypeCheckers

import Data.List (delete)



---- 1 - MATHEMATICAL FUNCTIONS ------------------------------------------------

    -- Classic mathematical function
choose :: Integral a => a -> a -> a
n `choose` k = product [k+1..n] `div` product [1..n-k]



---- 2 - PROBABILITY -> HAND FUNCTIONS -----------------------------------------

    -- Return the hand the player actually has
-- bestHand :: [Card] -> Hand
-- bestHand cs = probsToHand scs . snd $ foldr addCard noProbs scs
--     where scs = sort cs


    -- Assumes Probabilities are sorted by descending HandType
    -- and that at least one has a 100% chance
probsToHand :: [Card] -> [Prob] -> Hand
probsToHand scs prs = Hand hT scs
    where hT = pKind . head . dropWhile ((/= 1) . chance) $ prs



---- 3 - HAND SPECIFIC PROBABILITY FUNCTIONS -----------------------------------

    -- Change the existing probabilities on existing list of cards by taking a
    -- new card into consideration
-- addCard :: Card -> [Prob] -> [Prob]
-- addCard c (scs, [rF, sF, fK, fH, fl, st, tK, tP, oP, hC]) =
--              (c:scs, [rF', sF', fK', fH', fl', st', tK', tP', oP', hC'])
--     where (rF', sF', fl')           = flushesProbs  (rF, sF, fl) scs c
--           (fK', fH', tK', tP', oP') = nOfAKindProbs (fK, fH, tz, tP, oP) scs c
--           st'                       = straightProb  st scs c
--           hC'                       = highCardProb  hC scs c


--aimingFor :: [Prob] -> Prob
-- Perhaps this should just be bestChance

    -- Returns Probabilities of RoyalFlush, StraightFlush and Flush
-- flushesProbs :: (Prob,Prob,Prob) -> [Card] -> Card -> (Prob,Prob,Prob)
-- flushesProbs (rF, sF, fl) scs c = (rF', sF', fl')
--    where rF' =
--          sF' =
--          fl' = Prob Flush flChance flNeed
--
--          flChance = sum $ map check required
--              where n  = 52 - 2*(nPlayers - 1) - (length scs + 1)
--                    check x
--                      | x > left  = 0
--                      | otherwise = (1/) . choose n x
--              ---- NEED TO CARRY AROUND OR, IN GENERAL, KNOW WHAT SUIT IS BEING CONSIDERED.
--              ---- ALSO, NEED TO TAKE INTO ACCOUNT THE CONDITIONAL (A|B) PROBABILITY
--              ---- OF EXTRACTING THE NEEDED NUMBER OF CARDS OF THE SPECIFIC SUITS GIVEN
--              ---- THAT left CARDS WILL BE/HAVE BEEN EXTRACTED. (THE choose RIGHT
--              ---- BEFORE THESE COMMENTS SHOULD BE SUCH A CONDITIONAL ONE).
--
--              -- Cards left to extract in Texas Hold'em (one card is 'c')
--          left = 6 - length scs
--              ---- WRONG: NOT CONSIDERING DISCARDED CARDS, DIFFERENT AT EACH TURN.
--
--              -- Number of required cards of the same suits
--          required = map (5-) . map length $ suitGroups scs



    -- Returns Probabilities of FourOfAKind, FullHouse, ThreeOfAKind, TwoPair and OnePair
-- nOfAKindProbs :: (Prob,Prob,Prob,Prob,Prob) -> [Card] -> Card -> (Prob,Prob,Prob,Prob,Prob)
-- nOfAKindProbs (fK, fH, tz, tP, oP) scs c = (fK', fH', tK', tP', oP')
--    where fK' =
--          fH' =
--          tK' =
--          tP' =
--          oP' =



    -- Returns the Probability of a Straight
-- straightProb :: Prob -> [Card] -> Card -> Prob
-- straightProb st scs c =



    -- Returns the Probability of a HighCard
-- highCardProb :: Prob -> [Card] -> Card -> Prob
-- highCardProb hC scs c = Prob HighCard 1 []



---- 4 - QUALITY RELATED FUNCTIONS ---------------------------------------------

type Qual = Int
    -- Returns the Quality of a HighCard
highCardQual :: Hand -> Qual
highCardQual h = fromEnum . head . cards $ h



---- 5 - HANDTYPE INSTANCES COUNTERS -------------------------------------------

    -- Return the list of all HandTypes and how many "real " instances of each
    -- exist, i.e. taking into account the fact that if some cards constitute
    -- more than one HandType, they should count only as the highest one


    -- Return how many instances of a specific HandType exist


    -- Return all possible 5-card combinations ordered by SOME_ORDER
--allHandCombinations :: [[Card]]
--allHandCombinations = intsToCards $ combinations 5 allCards

combinations :: Int -> [Int] -> [[Int]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = concat $ map (\x -> map (x:) $ combinations (n-1) (delete x xs)) xs
    -- THIS IS WRONG: IT BEHAVES EXACTLY AS A LIST COMPREHENSION CHECKING THAT VALUES ARE DIFFERENT
    -- I.E. IT DOES NOT TAKE THE DONE VALUES OFF.
    -- REDO WITH A FOLD, POSSIBLY FOLDL', KEEPING THE "REMAINGIN" VALUES IN THE ACCUMULATOR



