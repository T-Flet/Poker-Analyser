---- Poker Analyser Probability Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.2 - 30-31/03/2015
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

import Data.List (tails)



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


    -- Return the list of all HandTypes and how many "real " instances of each
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


    -- Return all the "choose" combinations of length k from a list of length n
    -- NOTE: This is a direct translation of the mathematical recurrence
    -- relation of Binomial Coefficients
    -- NOTE: (length $ combinations k xs) == ((length xs) `choose` k)
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' ls@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [ls]
          | null ls   = []
          | otherwise = map (y:) nkMinus1 ++ nMinus1
            where nkMinus1 = combinations' (n-1) (k'-1) ys
                  nMinus1  = combinations' (n-1)  k'    ys



