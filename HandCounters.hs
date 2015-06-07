---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.15 - 06-07/06/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions related to counting possible
--          Hands from given conditions.
--
--   Sections:
--       0 - Imports
--       1 - HandType Instances Counters
--       2 - General Functions
--       3 - Testing Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

module HandCounters where

import DataTypes
import GeneralFunctions (choose, combinations, ascLength, subsetOf)

import Data.List (tails, group, groupBy, sort, sortBy, delete, nub, partition, union, (\\))
import qualified Data.Sequence as Seq (replicateM)
import Data.Foldable (toList)
import Data.Function (on)
import Control.Applicative ((<$>), (<*>))

import Control.Parallel.Strategies (using, parList, rdeepseq)


    -- For testing purposes (see Testing Functions)
import HandTypeCheckers



---- 1 - HANDTYPE INSTANCES CALCULATORS ----------------------------------------

    -- Each of the following functions returns the list of HandTypeCounts of possible
    -- instances of a specific HandType which can be obtained by completing a
    -- set of n cards, where n is each element of the given list of numbers.

    -- Their input is:
    --              [Int] -> Deck -> [Card] -> [Card] -> [HandTypeCount]
    -- multiple numbers of cards left to draw, the current Deck, the cards which
    -- should not be considered (like the player's if working just on the table)
    -- and the cards in question.

    -- Note: each function avoids counting instances of their HandType which also
    -- qualify as a different one (be it higher or lower)
    -- Also, if an instance of its HandType is already present, each function
    -- returns all the better or equal ones, excluding the ones which are worth
    -- the same but different (i.e. the present instane and all the strictly
    -- better ones; think of Straights of different sets of Suits)

    -- Note: most of these functions break down with unreasonable input (like
    -- lists of identical cards, which are not possible)


    -- Apply all HandTypes' Instances Calculators to the given cards
countHandTypes :: [Int] -> Deck -> [Card] -> [Card] -> [[HandTypeCount]]
countHandTypes tcns d ocs cs = foldl separate (replicate (length tcns') []) countsLists
    where   -- This fold is a list based unZipN
          separate acc counts = zipWith (++) acc $ group counts
          countsLists = map (\f-> f tcns' d ocs cs) countFunctions `using` parList rdeepseq
          countFunctions = [countRoyalFlush,
                            countStraightFlush,
                            countFourOfAKind,
                            countFullHouse,
                            countFlush,
                            countStraight,
                            countThreeOfAKind,
                            countTwoPair,
                            countOnePair,
                            countHighCard]
          tcns' = apprTcns tcns $ length ocs + length cs


countRoyalFlush = countPossHands RoyalFlush aphs
    where aphs = fromSVG allSuits (enumFrom Ten)


countStraightFlush tcns d ocs cs = countPossHands StraightFlush aphs tcns d ocs cs
    where aphs = filter ((`notElem` cs) . succ . last) phs
          phs = concat $ map (fromSVG allSuits) apvs
            -- Remove the Straights which, if present, are overshadowed by another
            -- greater by one (a Card of Value just above them is in cs)
            -- Note: no risk of error on succ because only 9 taken below
          apvs = filter ((`notElem` csvs) . succ . last) pvs
          csvs = cardsValues cs
            -- Taking 9 (init) and not 10 prevents RoyalFlushes
          pvs = init straightValues


countFourOfAKind = countPossHands FourOfAKind aphs
    where aphs = concat $ map (\val-> fromVSG [val] allSuits) allValues


countFullHouse tcns d ocs cs = countPossHands FullHouse aphs tcns d ocs cs
    where aphs = [makeCs [v3,v3,v3] ss3 ++ makeCs [v2,v2] ss2 | (ss3,ss2) <- apsss, (v3,v2) <- apvs, nF v3 ss3 v2 ss2]
            -- Ensuring v3 /= v2 prevents FourOfAKinds
          apvs = [(v3,v2) | v3 <- allValues, v2 <- allValues, v3 /= v2]
          apsss = [(ss3,ss2) | ss3 <- combinations 3 allSuits, ss2 <- combinations 2 allSuits]
          nF v3 ss3 v2 ss2 = noFourOfAKinds3 v3 ss3 && noFourOfAKinds2 v2 ss2
          noFourOfAKinds3 v ss = Card v (head (allSuits \\ ss)) `notElem` cs
            -- This one has a not-all-elem instead of an intuitive any-notElem
            -- because if there are 2 ThreeOfAKinds it is still a FullHouse
          noFourOfAKinds2 v ss = not . all (`elem` cs) $ makeCs (repeat v) (allSuits \\ ss)


countFlush tcns d ocs cs = countPossHands Flush aphs tcns d ocs cs
    where aphs = [fromSV [s] vs | vs <- apvs, s <- allSuits]
            -- Remove all FullHouses and FourOfAKinds
          apvs = filter ((>2) . length . group . sort) pvs
            -- Note that each combination below is sorted by ascending Value
          pvs = combinations 5 allValues \\ npvs
          npvs = implStrVs ++ explStrVs
            -- Implicit Straight Values (formed with the addition of the considered Cards)
          implStrVs = concat $ map getVs npCompletersVs
          getVs (origVs,vs) = map (\comVs-> sort (vs ++ comVs)) $ combinations (5 - length vs) (allValues \\ origVs)
          npCompletersVs = concat $ map getNpComplVs explStrVs
          getNpComplVs origVs = nub $ map (\vs-> (origVs, origVs \\ vs)) csvsCombs
          csvsCombs = concat $ map (flip combinations csvs) [1..length csvs]
          csvs = cardsValues cs
            -- Explicit Straight Values (has to be in this order to do so efficiently)
          explStrVs = (enumFromTo Two Five ++ [Ace]) : (tail straightValues)


countStraight tcns d ocs cs = countPossHands Straight aphs tcns d ocs cs
    where aphs = case isStraight cs of
            Just (_,strCs) -> eqOrBetter (value $ last strCs) phs
            _           -> phs
          eqOrBetter v = filter (all ((>=v) . value))
          phs = filter (\h-> not $ any (`subsetOf` h) npcs) phs'
            -- Remove all indirect StraightFlushes: when a Straight
            -- is acheived, but some adjacent cards create a StraightFlush)
          npcs = nub . concat $ map strFluCs cs
          strFluCs c = map (fromSV [suit c]) . strFluVs $ value c
          strFluVs v = map (delete v) $ filter (v `elem`) straightValues -- Used to be apvs instead of straightValues




          -- TEMPORARY BEFORE ADDING ANOTHER LAYER IN HERE!!!!!!!!!!
          phs' = concat $ map (zipWith (++) <$> repeat . concat . fst <*> snd) phsg

          phsg = [(cscs, ncss vs) | (vs,cscs) <- apvsAndCsInCsToUse]
          ncss vs = [makeCs vs ss | ss <- apsss !! length vs, noImplicitFlush ss]
          noImplicitFlush = all ((<5) . length) . group . sort . (csss++)
          csss = map suit cs

          apvsAndCsInCsToUse = map ((,) <$> (\\csvs) <*> csInCsVgs) straightValues
          csInCsVgs vs = valueGroups $ filter ((`elem` vs) . value) cs
          csvs = cardsValues cs

            -- The following is to ensure that after their first evaluation all
            -- 5 lists do not need to be regenerated
            -- (As opposed to using this every time: apss n = pss n \\ npss n)
          apsss = map ((\\) <$> pss <*> npss) [0..5]
            -- Remove all direct StraightFlushes (and Flushes)
          npss n = map (replicate n) allSuits
          pss  n = map toList $ Seq.replicateM n allSuits


countThreeOfAKind tcns d ocs cs = countPossHands ThreeOfAKind aphs tcns d ocs cs
    where aphs = case length nPletsVgs of
            x | x > 1  -> []
            1 -> case length hnpvgs of
                y | y > 3 -> []
                3 -> nPletsVgs
                    -- The below case can only be 2, but wildcard just in case
                _ -> [makeCs [v,v,v] (sort (s:ss)) | s <- allSuits \\ ss, noBetterHts s]
                        where noBetterHts s = noStraights cs [v] && noFlushes cs [s]
                              v = value $ head hnpvgs
                              ss = map suit hnpvgs
            _ -> [makeCs [v,v,v] (delete s allSuits) | v <- allValues, s <- allSuits, noBetterHts v s]
                        where noBetterHts v s = noFourOfAKinds v s &&
                                                noStraights cs [v] &&
                                                let vInCs = filter ((==v) . value) cs in
                                                if null vInCs
                                                    then noFlushes cs (delete s allSuits)
                                                    else noFlushes cs . (allSuits\\) $ map suit vInCs

          noFourOfAKinds v s = Card v s `notElem` cs

          hnpvgs = head nPletsVgs
          nPletsVgs = filter ((>1) . length) $ valueGroups cs


countTwoPair tcns d ocs cs = countPossHands TwoPair aphs tcns d ocs cs
    where aphs
                -- Any Explicit Three or Four OfAKind present
            | any ((>=3) . length) $ valueGroups cs = []
            | otherwise = case best2Nplets of
                [[c1,c2],[c3,c4]] -> eqOrBetter (value c3) phs
                _                 -> phs
          best2Nplets = head . groupBy ((==) `on` length) $ valueDescGroups cs
            -- Notice value c3 < value c1
          eqOrBetter v = filter (all ((>=v) . value))

          phs = [ncs | [v1,v2] <- apvs, ss1 <- apss, ss2 <- apss, let ncs = fromVS [v1] ss1 ++ fromVS [v2] ss2, noBetterHts ncs v1 v2 ss1 ss2]
          noBetterHts ncs v1 v2 ss1 ss2 = noStraights cs [v1,v2] && noFlushes' ncs && noNPlets ncs [v1,v2]
            -- Implicit Three or Four OfAKinds
          noNPlets ncs vs = not $ any (`elem` cardsValues (cs\\ncs)) vs
            -- Implicit Flushes
          noFlushes' ncs = not . any ((>=5) . length) . suitGroups $ union cs ncs

          apvs = combinations 2 allValues
          apss = combinations 2 allSuits



countOnePair tcns d ocs cs = countPossHands OnePair aphs tcns d ocs cs
    where aphs
                -- Any Straight Present
            | any (`subsetOf` csvs) straightValues = []
            | null nPletsVgs                       = csvshs ++ ncsvshs
            | pairCs@[c1,c2] <- head vdgs          = [pairCs]
            | otherwise                            = []
          csvshs = [[Card v s] | v <- csvs, s <- allSuits, noFlushes cs [s]]
          ncsvshs = [fromVS [v] ss | v <- allValues \\ csvs, ss <- combinations 2 allSuits, noBetterHts cs v ss]

          noBetterHts cs v ss = noStraights cs [v] && noFlushes cs ss

          nPletsVgs = filter ((>1) . length) vdgs
          vdgs = valueDescGroups cs
          csvs = cardsValues cs


countHighCard tcns d ocs cs = countPossHands HighCard aphs tcns d ocs cs
    where aphs = group apcs
          apcs
                -- Any Straight Present
            | any (`subsetOf` csvs) stpvs          = []
                -- Any Flush present
            | any ((>4) . length) $ suitGroups cs  = []
                -- Any nPlet present
            | any ((>1) . length) $ valueGroups cs = []
                -- Otherwise just trim the possible Suits and Values sets
            | otherwise = maximum cs : [Card v s | v <- apvs, s <- apss]

          apvs = filter (>= maximum csvs) . (\\) allValues $ union straightVs nPletsVs
          apss = allSuits \\ flushSs

          nPletsVs = csvs

          (fss, svs)
            | length cs >= 4 = (flushSs, straightVs)
            | otherwise      = ([],[])

          flushSs = map (suit . head) . filter ((>=4) . length) $ suitGroups cs
          straightVs = concat . filter ((==1) . length) $ map (\\ (sort csvs)) stpvs
          stpvs = (enumFromTo Two Five ++ [Ace]) : (tail straightValues)

          csvs = cardsValues cs



---- 2 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Possible Hands narrowing down process common to all count functions
countPossHands :: HandType -> [[Card]] -> [Int] -> Deck -> [Card] -> [Card] -> [HandTypeCount]
countPossHands ht allPossHands tcns d outCs cs = map finalCount tcns'
    where tcns' = apprTcns tcns usedCsNum
          finalCount tcn
            | csLeft > 0 = HandTypeCount ht possHands tcn countTuples
            | otherwise  = HandTypeCount ht []  tcn []
                where csLeft = tcn - usedCsNum
                      countTuples = map (\l-> (length l, head l)) $ group hProbs
                      hProbs = map (handProb csLeft d) possHands
                      possHands = sortBy ascLength $ filter ((<= csLeft) . length) neededHands
          neededHands = map (\\cs) notOcsHands
          notOcsHands = filter (not . any (`elem` outCs)) allPossHands
          usedCsNum = length outCs + length cs


    -- Return the appropriate Target Card NumberS for a given number of cards
    -- which are already in play
apprTcns :: [Int] -> Int -> [Int]
apprTcns tcns usedCsNum
    | null tcns = dropWhile (<usedCsNum) [2,5,6,7]
    | otherwise = tcns
    -- Same thing but given the current round stage
stageTcns :: Action -> [Int]
stageTcns stage = case stage of
   RoundStart -> [2,5,6,7]
   Flop  _    ->   [5,6,7]
   Turn  _    ->     [6,7]
   River _    ->       [7]


    -- Return the probability of drawing the given Cards from the given Deck in
    -- the given number of draws
    -- EVOLVE THIS INTO USING ALL THE VALUES IN Deck
handProb :: (Fractional a) => Int -> Deck -> [Card] -> a
handProb csl d cs
    | null cs   = fromIntegral 1
    | otherwise = ((/) `on` fromIntegral) drawsWithCs allPossDraws
        where -- The ok draws are the ones which contain the h cards and any of the
              -- possible sets of (k-h) cards wich can be made from the remaining (n-h) cards
              drawsWithCs  = n - h `choose` k - h
              allPossDraws = n     `choose` k
              -- h <= k <= n
              n = cardsIn d
              k = csl
              h = length cs


    -- List of Straight Values
straightValues :: [[Value]]
straightValues = take 10 . map (take 5) . tails $ Ace:allValues


    -- True if no Flushes are possible by adding the input Suits to the input Cards
noFlushes :: [Card] -> [Suit] -> Bool
noFlushes cs ss = not . any ((>=5) . length) . group $ sort (ss ++ map suit cs)
    -- True if no Straights are possible by adding the input Values to the input Cards
noStraights :: [Card] -> [Value] -> Bool
noStraights cs vs = not $ any ((`subsetOf` cardsValues cs) . (\\vs)) straightValues
    -- Values of the given Cards
cardsValues :: [Card] -> [Value]
cardsValues cs = nub $ map value cs



---- 3 - TESTING FUNCTIONS -----------------------------------------------------

    -- Test whether any of the count functions yields a HandType which is not
    -- its own. In particular, lookout for ones higher than it
    -- Also, count the instances of each
checkAllHtCs :: [Int] -> Deck -> [Card] -> [Card] -> [(HandType, [[HandType]])]
checkAllHtCs tcns d ocs cs = filter (not . null . snd) . map (notNulls . bad . getChecks) . reverse $ enumFrom HighCard
    where notNulls (ht,badChts) = (ht, filter (not . null) badChts)
          bad (ht,chts) = (ht, map (nub . sort . filter (/= ht)) chts)
          getChecks ht = (ht, checkHtc ht tcns d ocs cs)
    -- Parallel version
checkAllHtCsPar :: [Int] -> Deck -> [Card] -> [Card] -> [(HandType, [[HandType]])]
checkAllHtCsPar tcns d ocs cs = list `using` parList rdeepseq
    where list = checkAllHtCs tcns d ocs cs


    -- Map each HandType to its counter function
getHtcFunc :: HandType -> ([Int] -> Deck -> [Card] -> [Card] -> [HandTypeCount])
getHtcFunc ht = case ht of
    RoyalFlush      -> countRoyalFlush
    StraightFlush   -> countStraightFlush
    FourOfAKind     -> countFourOfAKind
    FullHouse       -> countFullHouse
    Flush           -> countFlush
    Straight        -> countStraight
    ThreeOfAKind    -> countThreeOfAKind
    TwoPair         -> countTwoPair
    OnePair         -> countOnePair
    HighCard        -> countHighCard


    -- Return all the completers which yield different HandTypes than the required one
filterBad :: HandType -> [Int] -> Deck -> [Card] -> [Card] -> [[[Card]]]
filterBad ht tcns d ocs cs = map (filter ((/=ht) . checkSingle cs) . completers) $ (getHtcFunc ht) tcns d ocs cs
    -- Return all the HandTypes yielded by each completer
checkHtc :: HandType -> [Int] -> Deck -> [Card] -> [Card] -> [[HandType]]
checkHtc  ht tcns d ocs cs = map (map    (checkSingle cs)          . completers) $ (getHtcFunc ht) tcns d ocs cs
    -- Return the HandType yielded by a completer
checkSingle cs = hType . bestHandType . union cs

