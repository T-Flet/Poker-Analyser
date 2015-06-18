---- Poker Analyser Abstract Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.3 - 18/06/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions related to counting possible
--          Hands from given conditions without generating these hands while
--          counting (i.e. purely mathematically).
--
--   Sections:
--       0 - Imports
--       1 - Complete HandType Instances Counters
--       2 - Single HandType Instances Counters
--       3 - General Functions
--       4 - Testing Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module HandCounters where

import GeneralFunctions (choose)
import DataTypes
import HandTypeCheckers

import Data.List (nub, tails, (\\))
import Control.Applicative ((<$>), (<*>))



---- 1 - COMPLETE HANDTYPE INSTANCES CALCULATORS -------------------------------




    -- Map each HandType to its abstract counter function
getAbHtcFunc ht = case ht of
    RoyalFlush    -> abstrCountRoyalFlush
--    StraightFlush -> abstrCountStraightFlush
--    FourOfAKind   -> abstrCountFourOfAKind
--    FullHouse     -> abstrCountFullHouse
--    Flush         -> abstrCountFlush
--    Straight      -> abstrCountStraight
--    ThreeOfAKind  -> abstrCountThreeOfAKind
--    TwoPair       -> abstrCountTwoPair
--    OnePair       -> abstrCountOnePair
--    HighCard      -> abstrCountHighCard



---- 2 - SINGLE HANDTYPE INSTANCES CALCULATORS ---------------------------------

    -- Call a single abstract HandType counter, without manually inputting all
    -- the data yielded by the hand analysis functions

    -- EVENTUALLY MAKE tcn INTO tcns
singleAbstrHtCount :: HandType -> Int -> Deck -> [Card] -> [Card] -> Int
singleAbstrHtCount ht tcn d ocs cs = getAbHtcFunc ht tcn d ocs cs hd hs
    where hd = handData tcn ocs cs
          hs = handStats d ocs cs


    -- Each of these functions takes as input:
    --      The target number of cards which will be drawn by the end
    --      The current state of the Deck
    --      The Cards which are not supposed to be considered (e.g. someone else has them)
    --      The Cards which have already been drawn
    --      The data returned by the hand analysis functions
    -- And they return the number of hands of n cards which are their HandType

    -- NOTE: These functions are guaranteed to make sense up to 7 target cards



abstrCountRoyalFlush tcn d ocs cs (lid,ltd,ad) (hand,vdgs,sdgs) =
                                    htCheck RoyalFlush (hType hand) itIs itIsNot
    where itIs = lid `choose` ltd
          itIsNot = sum $ map ((choose <$> (lid-) <*> (ltd-)) . length) phs
          phs = map (\\ cs) $ fromSVG (allSuits \\ npss) (enumFrom Ten)
          npss = nub . map suit $ filter ((>=Ten) . value) ocs


--abstrCountStraightFlush tcn d ocs cs (lid,ltd,ad) (hand,vdgs,sdgs) =
--                                    htCheck StraightFlush (hType hand) itIs itIsNot
--    where itIs = (lid `choose` ltd) - ((choose <$> (lid-) <*> (ltd-)) diff)
--          diff = ((-) `on` fromEnum) Ace . toV $ hTField hand
--
--          itIsNot =







---- 3 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Structure common to all abstract counters
htCheck :: HandType -> HandType -> Int -> Int -> Int
htCheck ht csHt itIs itIsNot
    | csHt == ht = itIs
    | otherwise  = itIsNot


    -- Data extraction common to all abstract counters
handData :: Int -> [Card] -> [Card] -> (Int,Int,Int)
handData tcn ocs cs = (leftInDeck, leftToDraw, alreadyDrawn)
    where leftInDeck = 52  - alreadyDrawn
          leftToDraw = tcn - alreadyDrawn
          alreadyDrawn = length ocs + length cs


    -- Complete Analysis of the given situation cards-wise; it saves each
    -- abstract counter doing its own checks.
    -- Among other things it returns:
    --      which HandType the cards constitute
    --      the Cards by descending Suit and Value groups

    --        NOT YET IMPLEMENTED
    --      whether any specific HandType is impossible
handStats :: Deck -> [Card] -> [Card] -> (Hand,[[Card]],[[Card]])
handStats d ocs cs = (hand, vdgs, sdgs)
    where hand = bestHandType cs
          vdgs = valueDescGroups cs
          sdgs = suitDescGroups cs


    -- List of Straight Values
straightValues :: [[Value]]
straightValues = take 10 . map (take 5) . tails $ Ace:allValues



---- 4 - TESTING FUNCTIONS -----------------------------------------------------



