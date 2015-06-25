---- Poker Analyser QuickCheck, Ultimate Testing and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          1.0 - 09-10/06/2015
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

import qualified GeneralFunctions as GF (combinations, choose)
import DataTypes
import HandCounters

import Test.QuickCheck
import Data.List (nub)
import Data.Function (on)
import Control.Parallel.Strategies



---- 1 - HANDTYPECOUNTERS CHECKS -----------------------------------------------

instance Arbitrary Value where
    arbitrary = elements allValues

instance Arbitrary Suit where
    arbitrary = elements allSuits

instance Arbitrary Card where
    arbitrary = elements allCards



-- Properties --

    -- Reasonable conditions on ocs and cs
propConds prop ocs cs =
    length ocs <= 2 ==>
    length cs  <= 7 ==>
    length ocs + length cs <= 7 ==>
        -- Lists of unique elements
    nub ocs == ocs && nub cs == cs ==>
        prop ocs cs


checkBetterHandTypesProp ocs cs = propConds prop ocs cs
    where prop ocs cs = null $ ress ocs cs
          ress = checkBetterHandTypes

checkAllHandTypesProp ocs cs = propConds prop ocs cs
    where prop ocs cs = null $ ress ocs cs
          ress = checkAllHandTypes

    -- Single Ht property check
checkSingleHtProp ht ocs cs = propConds prop ocs cs
    where prop ocs cs = all null $ ress ocs cs
          ress = filterBad ht


    -- Test the given HandTypes' poss functions against all possible combinations of 7 Cards
checkEverythingFor :: [HandType] -> IO ()
checkEverythingFor hts = do
    let allCombinations = GF.combinations 7 allCards `using` rdeepseq
    putStrLn "All combinations cached (look at your RAM, XD)"
    putStrLn "The testing will now happen in repeating parallel threads of 100 tests each"
    putStrLn "There will now be a very long pause without writing during testing"
    putStrLn "Good luck!! XD"

    let result = bools allCombinations

    if null result
        then putStrLn "SUCCESS!!!\nEverything works!!! This is AMAZING!!!"
        else do
                let n = length result
                putStrLn $ "Stopped at the first failure, which was the hand number: " ++ show n
                let percentage = (((/) `on` fromIntegral) n $ 52 `GF.choose` 7) :: Float
                putStrLn $ "This means that " ++ show percentage ++ "% of the combinations were evaluated to be ok"
                putStrLn "The problematic combination is:"
                putStrLn . show $ allCombinations!!n
                putStrLn "Please send me this result"
        where bools = takeWhile (==True) . withStrategy (parListChunk 100 rdeepseq) . needToBeTrue
              needToBeTrue allCombs = map (null . checkHandTypes hts []) allCombs



---- 2 - COMPILED TESTING SHELL STUFF ------------------------------------------

    -- Compile with: ghc -o PokerTesting -O QuickCheckStuff
    -- Or, Multi Core: ghc -o PokerTestingNCores -O QuickCheckStuff -threaded +RTS -N
    -- Then delete all the intermediate files in the repo

main = do
    putStrLn "\nDr_lord's Poker Analyser: HandType counting functions testing"
    putStrLn "Meant for Isaac Jordan, and Ben Jackson 09/06/2015"
    checkInput

checkInput = do
    putStrLn "\nPlease enter either 'quit', 'all', 'better', 'ULTIMATE' or any single HandType's name: "
    putStrLn $ show allHandTypes
    putStrLn "'all' and 'better' are referred to the HandType constituted by the given cards"
    putStrLn "Note that ULTIMATE will take a long time (a little for each of the 133784560 (52 `choose` 7) sets of cards)"
    cmd <- getLine
    case cmd of
        "quit" -> do
                    putStrLn "\nThank you for testing. Please send me a print of your results, XD"
                    putStrLn "The important part usually is just the last 3 lines; it will be something like 'True because...' or 'Falsifiable after...'"
        "all" -> do
                    quickCheckWith stdArgs { maxSuccess = 10000 } checkAllHandTypesProp
                    checkInput
        "better" -> do
                    quickCheckWith stdArgs { maxSuccess = 10000 } checkBetterHandTypesProp
                    checkInput
        "ultimate" ->
                    do
                        checkEverythingFor allHandTypes
        x
            | x `elem` map show allHandTypes -> do
                quickCheckWith stdArgs { maxSuccess = 10000 } $ checkSingleHtProp (read x :: HandType)
                checkInput
            | otherwise -> do
                putStrLn "Unrecognised input"
                checkInput




