---- Poker Analyser Abstract Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.2 - 10/06/2015
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
    --      The current state of the Deck
    --      The Cards which are not supposed to be considered (e.g. someone else has them)
    --      The Cards which have already been drawn
    -- And they return the number of hands of n cards which are their HandType

abstrCountRoyalFlush :: Int -> Deck -> [Card] -> [Card] -> Int
abstrCountRoyalFlush n d ocs cs = htCheck RoyalFlush cs itIs itIsNot
    where (lid,ltd,ad) = handData n ocs cs
          itIs = lid `choose` ltd
          itIsNot = sum $ map ((choose <$> (lid-) <*> (ltd-)) . length) phs
          phs = map (\\ cs) $ fromSVG (allSuits \\ npss) (enumFrom Ten)
          npss = nub . map suit $ filter ((>=Ten) . value) ocs







---- 3 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Structure common to all abstract counters

        -- EVENTUALLY SUBSTITUTE THIS WITH A VALUE FROM handStats
htCheck :: HandType -> [Card] -> Int -> Int -> Int
htCheck ht cs itIs itIsNot
    | hType (bestHandType cs) == ht = itIs
    | otherwise                     = itIsNot


    -- Data extraction common to all abstract counters
handData :: Int -> [Card] -> [Card] -> (Int,Int,Int)
handData n ocs cs = (leftInDeck, leftToDraw, alreadyDrawn)
    where leftInDeck = 52 - alreadyDrawn
          leftToDraw = n  - alreadyDrawn
          alreadyDrawn = length ocs + length cs


    -- Complete Analysis of the given situation cards-wise; it saves each
    -- abstract counter doing its own checks.
    -- Among other things it returns:
    --      which HandType the cards constitute
    --      whether any specific HandType is impossible

        -- PROBABLY ADD MOST CHECKS HERE AS ABSTRACT COUNTERS ARE WRITTEN
--handStats :: Deck -> [Card] -> [Card] -> (,,)
--handStats d ocs cs = (ht, )
--    where ht = hType $ bestHandType cs



---- 4 - TESTING FUNCTIONS -----------------------------------------------------



