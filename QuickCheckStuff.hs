---- Poker Analyser QuickCheck and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.2 - 27-28/05/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the Arbitrary type instances and QuickCheck
--          related properties and functions.
--
--   Sections:
--       0 - Imports
--       1 - Card Related Checks
--       2 - Hand Related Checks
--       3 - State Related Checks
--



---- 0 - IMPORTS ---------------------------------------------------------------

module QuickCheckStuff where

import DataTypes
import HandCounters

import Test.QuickCheck



---- 1 - CARD RELATED CHECKS ---------------------------------------------------

instance Arbitrary Value where
    arbitrary = elements allValues

instance Arbitrary Suit where
    arbitrary = elements allSuits

instance Arbitrary Card where
    arbitrary = elements allCards













checkBetterProp ocs cs =
    length ocs <= 2 ==>
    length cs  <= 7 ==>
    length ocs + length cs <= 7 ==>
        null ress
    where ress = checkBetter initialDeck ocs cs





