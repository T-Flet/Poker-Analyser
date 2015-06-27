---- Poker Analyser QuickCheck, Ultimate Testing and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          1.1 - 26-27/06/2015
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
import GHC.Conc (numCapabilities)
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


    -- Test the given HandTypes' poss functions against the possible combinations
    -- of 7 Cards from the given percentage (e.g. 0 means from the beginning)
checkEverythingFor :: [HandType] -> Float -> IO ()
checkEverythingFor hts perc = do
    let startNum = (floor ((perc  / 100.0) * acn)) :: Int
    let allCombinations = GF.combinations 7 allCards `using` evalList rdeepseq
    putStrLn "All combinations cached (look at your RAM, XD)"
    putStrLn $ "The starting combination number corresponding to the " ++ show perc ++ "% is: " ++ show startNum
    putStrLn "The testing will now happen in repeating parallel threads of N tests each"
    putStr "What should the value of N be (100 might be good)? (Int): "
    tTNStr <- getLine
    let tTN = read tTnStr :: Int -- threadTestNumber
    putStrLn "There will now be a long pause without writing during each batch of testing threads"
    putStrLn "Good luck!! XD"

    testingBatch acn startNum tTN $ drop startNum allCombinations
        where acn = 52 `GF.choose` 7


testingBatch :: Int -> Int -> Int -> [[Card]] -> IO ()
testingBatch acn currentNum tTN combs = do
    let batchLength = numCapabilities * tTN
    let (thisBatch,newCombs) = splitAt batchLength combs

    let bools = map (null . curriedCheck) thisBatch `using` parListChunk tTN rdeepseq
        where curriedCheck h = checkHandTypes nhts [] h
                where nhts
                        | null hts  = enumFromThenTo RoyalFlush StraightFlush . hType $ bestHandType hts
                        | otherwise = hts

    let percentage = ((((/) `on` fromIntegral) resNum acn) :: Float) * 100.0
        l = length $ takeWhile (==True) bools
        resNum = currentNum + l
    in if l == batchLength
        then do
            putStrLn $ "OK up to " ++ show newNum ++ " : " ++ show percentage ++ "%"
            testingBatch newNum tTN newCombs
                where newNum = currentNum + batchLength
        else do
                putStrLn $ "Stopped at first failure, which was hand number: " ++ show resNum
                putStrLn $ "This means that " ++ show percentage ++ "% of the combinations were evaluated to be ok"
                putStrLn "The problematic combination is:"
                putStrLn . show $ combs!!l
                putStrLn "Please send me this result"



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
                        putStrLn "From what percentage of all combinations do you want to start testing?"
                        putStrLn "(Note that the process will assume that all the combinations before that percentage check out)"
                        putStr "(Float): "
                        percStr <- getLine
                        let perc = read percStr :: Float
                        checkEverythingFor allHandTypes
        x
            | x `elem` map show allHandTypes -> do
                quickCheckWith stdArgs { maxSuccess = 10000 } $ checkSingleHtProp (read x :: HandType)
                checkInput
            | otherwise -> do
                putStrLn "Unrecognised input"
                checkInput




