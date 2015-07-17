---- Poker Analyser QuickCheck, Ultimate Testing and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          1.4 - 16-17/07/2015
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
--       3 - General Functions Stuff
--



---- 0 - IMPORTS ---------------------------------------------------------------

--module QuickCheckStuff where

import qualified GeneralFunctions as GF (combinations, combinationsFrom, choose)
import DataTypes
import HandTypeCheckers (bestHandType)
import HandCounters

import Test.QuickCheck
import Data.List (nub, sort)
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
          ress = checkBetterHandTypesPar

--checkAllHandTypesProp ocs cs = propConds prop ocs cs
--    where prop ocs cs = null $ ress ocs cs
--          ress = checkAllHandTypes

    -- Single Ht property check
checkSingleHtProp ht ocs cs = propConds prop ocs cs
    where prop ocs cs = all null $ ress ocs cs
          ress = filterBad ht


    -- Test the given HandTypes' poss functions against the possible combinations
    -- of 7 Cards from the given percentage (e.g. 0 means from the beginning)
--checkEverythingFor :: [HandType] -> [Card] -> Int -> IO ()
--checkEverythingFor hts startComb startNum = do
checkEverythingFor :: [Card] -> Int -> IO ()
checkEverythingFor startComb startNum = do
    putStrLn $ "\nThe exact starting combination percentage corresponding to the number " ++ show startNum ++ " is " ++ show perc ++ "%"
    putStrLn "The testing will now happen in repeating parallel threads of N tests each"
    putStrLn "What should the value of N be (100 might be good)? (Int): "
    tTNStr <- getLine
    let tTN = read tTNStr :: Int -- threadTestNumber
    putStrLn "\nThere will now be a pause without writing during each batch of testing threads"
    putStrLn "Good luck!! XD"

--    let nhts h
--            | null hts  = enumFromThenTo RoyalFlush StraightFlush . hType $ bestHandType h
--            | otherwise = hts

--    testingBatch nhts allCombsNum startNum tTN combsFrom
    testingBatch allCombsNum startNum tTN combsFrom
        where combsFrom = GF.combinationsFrom 7 allCards startComb
              perc = percentage startNum allCombsNum
              allCombsNum = 52 `GF.choose` 7


--testingBatch :: ([Card] -> [HandType]) -> Int -> Int -> Int -> [[Card]] -> IO ()
--testingBatch nhts allCombsNum currentNum tTN combs = do
testingBatch :: Int -> Int -> Int -> [[Card]] -> IO ()
testingBatch allCombsNum currentNum tTN combs = do
    let batchLength = numCapabilities * tTN
    let (thisBatch,newCombs) = splitAt batchLength combs

--    let curriedCheck h = checkHandTypes (nhts h) [] h
    let curriedCheck h = checkBetterHandTypes [] h
    let bools = map (null . curriedCheck) thisBatch `using` parListChunk tTN rdeepseq

    let l = length $ takeWhile (==True) bools
    let resNum = currentNum + l
    let newPerc = percentage resNum allCombsNum
    if l == batchLength
        then do
            putStrLn $ "OK up to " ++ show resNum ++ " : (" ++ show newPerc ++ ")%"
--            testingBatch nhts allCombsNum resNum tTN newCombs
            testingBatch allCombsNum resNum tTN newCombs
        else do
            putStrLn $ "\nStopped at first failure, which was hand number: " ++ show resNum
            putStrLn $ "This means that " ++ show newPerc ++ "% of the combinations were evaluated to be ok"
            putStrLn "The problematic combination is:"
            print $ combs!!l
            putStrLn "Please send me this result"

--- Helper Functions ---

percentage x tot = (((/) `on` fromIntegral) x tot :: Float) * 100.0


halfWayCs = ["6s", "ks", "kc", "2d", "5h", "6h"]
indexCs :: Int -> [Card]
indexCs ind = GF.combinations 7 allCards !!ind
combsIndex :: Float -> Int
combsIndex perc = round (perc * fromIntegral (52 `GF.choose` 7))



---- 2 - COMPILED TESTING SHELL STUFF ------------------------------------------

    -- Compile with: ghc -o PokerTesting -O TestingStuff
    -- Or, Multi Core: ghc -o PokerTestingNCores -O TestingStuff -threaded +RTS -N
    -- Then delete all the intermediate files in the repo

main = do
    putStrLn "\nDr_lord's Poker Analyser: HandType counting functions testing"
    putStrLn "Meant for Isaac Jordan, and Ben Jackson 09/06/2015"
    checkInput

checkInput = do
    putStrLn "\nPlease enter either 'quit', 'normal', 'ULTIMATE' or any single HandType's name: "
    print allHandTypes
                -- Substituted 'all' and 'better' with 'normal' in above print
--    putStrLn "'all' and 'better' are referred to the HandType constituted by the given cards"
    putStrLn "Note that ULTIMATE will take a long time (a little for each of the 133784560 (52 `choose` 7) sets of cards)"
    cmd <- getLine
    case cmd of
        "quit" -> do
                    putStrLn "\nThank you for testing. Please send me a print of your results, XD"
                    putStrLn "The important part usually is just the last 3 lines; it will be something like 'True because...' or 'Falsifiable after...'"
--        "all" -> do
--                    quickCheckWith stdArgs { maxSuccess = 10000 } checkAllHandTypesProp
--                    checkInput
--        "better" -> do
        "normal" -> do
                    quickCheckWith stdArgs { maxSuccess = 10000 } checkBetterHandTypesProp
                    checkInput
        "ultimate" ->
                    do
                        putStrLn "\nHow do you wish to determine from which combination to start testing?:"
                        putStrLn "(Note that the process will assume that all the combinations before that check out)"
                        putStrLn "By percentage or a precise combination and its index? ('perc'/'comb'): "
                        methodStr <- getLine
                        case methodStr of
                            "perc" -> do
                                putStrLn "Specific percentage? (Float (0.5 for 50%)): "
                                percStr <- getLine
                                let perc = read percStr :: Float
                                putStrLn "Determining the exact startpoint..."
                                let percInd = combsIndex perc
                                let percComb = indexCs percInd
                                putStrLn $ "\nCombination Number " ++ show percInd ++ " : " ++ show (map fromCard percComb)
                                checkEverythingFor percComb percInd
                            "comb" -> do
                                 putStrLn "\nFrom what combination do you want to start testing?"
                                 putStrLn "(Both the actual combination and its number in the ordered list of all of them are required)"
                                 putStrLn "[7 comma-separated 2-char \"strings\" representing cards] (or '[]' for beginning): "
                                 csStr <- getLine
                                 let Just startComb = mapM toCard (read csStr :: [String])
                                 putStrLn "\nCombination Number (Int): "
                                 numStr <- getLine
                                 let startNum = read numStr :: Int
--                               putStrLn "\nDo you want this test to run only on the HandTypes better than or equal to what the Card combinations constitutes ('better')?"
--                               putStrLn "Or do you want to test all HandTypes for each combination ('all')? : "
--                               allOrBetter <- getLine
--                               let hts = case allOrBetter of
--                                           "all" -> allHandTypes
--                                           "better" -> []
--                               checkEverythingFor hts startComb startNum
                                 checkEverythingFor startComb startNum
        x
            | x `elem` map show allHandTypes -> do
                quickCheckWith stdArgs { maxSuccess = 10000 } $ checkSingleHtProp (read x :: HandType)
                checkInput
            | otherwise -> do
                putStrLn "Unrecognised input"
                checkInput



---- 3 - GENERAL FUNCTIONS STUFF -----------------------------------------------

    -- All combinationsFrom results are a tail of combinations results with the
    -- same inputs
    -- TEST: quickCheckWith stdArgs { maxSuccess = 10000 } checkCombinations
checkCombinations xs startComb =
    nub xs == xs ==>
    nub startComb == startComb ==>
        combsFrom == dropCombs
    where combsFrom = GF.combinationsFrom k sxs ssc
          dropCombs = dropWhile (/= ssc) $ GF.combinations k sxs
          k = length startComb
          sxs = sort xs
          ssc = sort startComb
