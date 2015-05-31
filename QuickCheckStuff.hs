---- Poker Analyser QuickCheck and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.4 - 30-31/05/2015
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
    where ress = checkAllHtCs initialDeck ocs cs



---- 2 - COMPILED TESTING SHELL STUFF ------------------------------------------

    -- Compile with: ghc -o PokerTestingIsaac -O QuickCheckStuff
    -- Then delete all the intermediate files in the repo

main = do
    putStrLn "\nDr_lord's Poker Analyser: HandType counting functions preliminary testing"
    putStrLn "Meant for Isaac Jordan, 31/05/2015"
    checkInput

checkInput = do
    putStrLn "\nPlease enter either 'quit', 'quick' or 'verbose'"
    putStrLn "(Use 'verbose' if you feel like looking at what the tests are on and how they are going):"
    cmd <- getLine
    case cmd of
        "quit" -> do
                    putStrLn "\nThank you for testing. Please send me a print of your results, XD"
                    putStrLn "The important part usually is just the last 3 lines; it will be something like 'True because...' or 'Falsifiable after...'"
        "quick" -> do
                    quickCheckWith stdArgs { maxSuccess = 100000 } checkAllHtCsProp
                    checkInput
        "verbose" -> do
                    verboseCheckWith stdArgs { maxSuccess = 100000 } checkAllHtCsProp
                    checkInput
        _ -> do
                putStrLn "Unrecognised input"
                checkInput



