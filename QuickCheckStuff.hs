---- Poker Analyser QuickCheck and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.6 - 06/06/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the Arbitrary type instances and QuickCheck
--          related properties and functions.
--
--   Sections:
--       0 - Imports
--       1 - HandTypeCounters Checks
--       2 - Compiled Testing Shell Stuff
--



---- 0 - IMPORTS ---------------------------------------------------------------

--module QuickCheckStuff where

import DataTypes
import HandCounters

import Test.QuickCheck
import Data.List (nub)



---- 1 - HANDTYPECOUNTERS CHECKS -----------------------------------------------

instance Arbitrary Value where
    arbitrary = elements allValues

instance Arbitrary Suit where
    arbitrary = elements allSuits

instance Arbitrary Card where
    arbitrary = elements allCards



-- Properties --

checkAllHtCsProp ocs cs =
    length ocs <= 2 ==>
    length cs  <= 7 ==>
    length ocs + length cs <= 7 ==>
        -- Lists of unique elements
    nub ocs == ocs && nub cs == cs ==>
        null ress
    where ress = checkAllHtCs [2,5,6,7] initialDeck ocs cs

    -- Parallel version
checkAllHtCsParProp ocs cs =
    length ocs <= 2 ==>
    length cs  <= 7 ==>
    length ocs + length cs <= 7 ==>
        -- Lists of unique elements
    nub ocs == ocs && nub cs == cs ==>
        null ress
    where ress = checkAllHtCsPar [2,5,6,7] initialDeck ocs cs



---- 2 - COMPILED TESTING SHELL STUFF ------------------------------------------

    -- Compile with: ghc -o PokerTesting -O QuickCheckStuff
    -- Or, Multi Core: ghc -o PokerTestingNCores -O QuickCheckStuff -threaded +RTS -N
    -- Then delete all the intermediate files in the repo

main = do
    putStrLn "\nDr_lord's Poker Analyser: HandType counting functions testing"
    putStrLn "Meant for Isaac Jordan, and Ben Jackson 06/06/2015"
    checkInput

checkInput = do
    putStrLn "\nPlease enter either 'quit', 'normal' or 'parallel'"
    cmd <- getLine
    case cmd of
        "quit" -> do
                    putStrLn "\nThank you for testing. Please send me a print of your results, XD"
                    putStrLn "The important part usually is just the last 3 lines; it will be something like 'True because...' or 'Falsifiable after...'"
        "normal" -> do
                    quickCheckWith stdArgs { maxSuccess = 10000 } checkAllHtCsProp
                    checkInput
        "parallel" -> do
                    quickCheckWith stdArgs { maxSuccess = 10000 } checkAllHtCsParProp
                    checkInput
        _ -> do
                putStrLn "Unrecognised input"
                checkInput



