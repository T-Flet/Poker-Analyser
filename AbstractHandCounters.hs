---- Poker Analyser Abstract Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.1 - 10/06/2015
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

import Data.List (nub, (\\))
import Control.Applicative ((<$>), (<*>))



---- 1 - COMPLETE HANDTYPE INSTANCES CALCULATORS -------------------------------


---- 2 - SINGLE HANDTYPE INSTANCES CALCULATORS ---------------------------------

    -- Each of these functions takes as input:
    --      The number n of cards which will be drawn by the end
    --      The Cards which are not supposed to be considered (e.g. someone else has them)
    --      The Cards which have already been drawn
    -- And they return the number of hands of n cards which are their HandType

abstrCountRoyalFlush :: Int -> [Card] -> [Card] -> Int
abstrCountRoyalFlush n ocs cs = htCheck RoyalFlush cs itIs itIsNot
    where (lid,ltd,ad) = handData n ocs cs
          itIs = lid `choose` ltd
          itIsNot = sum $ map ((choose <$> (lid-) <*> (ltd-)) . length) phs
          phs = map (\\ cs) $ fromSVG (allSuits \\ npss) (enumFrom Ten)
          npss = nub . map suit $ filter ((>=Ten) . value) ocs







---- 3 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Data extraction common to all abstract counters
handData :: Int -> [Card] -> [Card] -> (Int,Int,Int)
handData n ocs cs = (leftInDeck, leftToDraw, alreadyDrawn)
    where leftInDeck = 52 - alreadyDrawn
          leftToDraw = n  - alreadyDrawn
          alreadyDrawn = length ocs + length cs


    -- Structure common to all abstract counters
htCheck :: HandType -> [Card] -> Int -> Int -> Int
htCheck ht cs itIs itIsNot
    | hType (bestHandType cs) == ht = itIs
    | otherwise                     = itIsNot



---- 4 - TESTING FUNCTIONS -----------------------------------------------------



