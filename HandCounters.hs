---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.12 - 30-31/05/2015
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

import Data.List (tails, group, sort, sortBy, delete, nub, partition, union, (\\))
import qualified Data.Sequence as Seq (replicateM)
import Data.Foldable (toList)


    -- For testing purposes (see Testing Functions)
import HandTypeCheckers



---- 1 - HANDTYPE INSTANCES CALCULATORS ----------------------------------------

    -- Each of the following functions returns the HandTypeCount of possible
    -- instances of a specific HandType which can be obtained by completing a
    -- set of 7 cards. Their input is:

    -- Deck -> [Card] -> [Card] -> HandTypeCount
    -- the current Deck, the cards which should not be considered (like the
    -- player's if working just on the table) and the cards in question.

    -- Note: each function avoids counting instances of their HandType which
    -- qualify as a higher one as well.

    -- Note: most of these functions break down with unreasonable input (like
    -- lists of identical cards, which are not possible)


    -- Apply all HandTypes' Instances Calculators to the given cards
countHandTypes :: Deck -> [Card] -> [Card] -> [HandTypeCount]
countHandTypes d ocs cs = map (\f-> f d ocs cs) countFunctions
    where countFunctions = [countRoyalFlush,
                            countStraightFlush,
                            countFourOfAKind,
                            countFullHouse,
                            countFlush,
                            countStraight,
                            countThreeOfAKind,
                            countTwoPair,
                            countOnePair,
                            countHighCard]


countRoyalFlush = countPossHands RoyalFlush aphs
    where aphs = fromSVG allSuits (enumFrom Ten)


countStraightFlush d ocs cs = countPossHands StraightFlush aphs d ocs cs
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


countFullHouse d ocs cs = countPossHands FullHouse aphs d ocs cs
    where aphs = [makeCs [v3,v3,v3] ss3 ++ makeCs [v2,v2] ss2 | (ss3,ss2) <- apsss, (v3,v2) <- apvs, nF v3 ss3 v2 ss2]
            -- Ensuring v3 /= v2 prevents FourOfAKinds
          apvs = [(v3,v2) | v3 <- allValues, v2 <- allValues, v3 /= v2]
          apsss = [(ss3,ss2) | ss3 <- combinations 3 allSuits, ss2 <- combinations 2 allSuits]
          nF v3 ss3 v2 ss2 = noFourOfAKinds3 v3 ss3 && noFourOfAKinds2 v2 ss2
          noFourOfAKinds3 v ss = Card v (head (allSuits \\ ss)) `notElem` cs
            -- This one has a not-all-elem instead of an intuitive any-notElem
            -- because if there are 2 ThreeOfAKinds it is still a FullHouse
          noFourOfAKinds2 v ss = not . all (`elem` cs) $ makeCs (repeat v) (allSuits \\ ss)


countFlush d ocs cs = countPossHands Flush aphs d ocs cs
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


countStraight d ocs cs = countPossHands Straight aphs d ocs cs
    where aphs = filter (\h-> not $ any (`subsetOf` h) npcs) phs
            -- Remove all indirect StraightFlushes: when a Straight
            -- is acheived, but some adjacent cards create a StraightFlush)
          npcs = nub . concat $ map strFluCs cs
          strFluCs c = map (fromSV [suit c]) . strFluVs $ value c
          strFluVs v = map (delete v) $ filter (v `elem`) apvs

          phs = [makeCs vs ss | vs <- apvs, ss <- apss, noImplicitFlush ss]
          noImplicitFlush ss = all (<5) $ map checkNums allSuits
            where checkNums s = sLookup s sgNums + sLookup s ssNums
                    -- No need to call in Data.Map to use its lookup function
                  sLookup s nums = case filter ((==s) . fst) nums of [] -> 0 ; [(_,n)] -> n
                  ssNums = map (\sl-> (head sl, length sl)) . reverse . group $ sort ss
                  sgNums = map (\sg-> (suit $ head sg, length sg)) $ suitGroups cs

          apvs = straightValues
          apss = pss \\ npss
            -- Remove all direct StraightFlushes (and Flushes)
          npss = map (replicate 5) allSuits
          pss = map toList $ Seq.replicateM 5 allSuits


countThreeOfAKind d ocs cs = countPossHands ThreeOfAKind aphs d ocs cs
    where aphs = case length nPletsVgs of
            x | x > 1  -> []
            1 -> case length hnpvgs of
                y | y > 3 -> []
                3 -> nPletsVgs
                _ -> [makeCs [v,v,v] (sort (s:ss)) | s <- allSuits \\ ss, noBetterHts v s] -- A single pair
                        where noBetterHts v s = noStraights cs [v] && noFlushes cs [s]
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


countTwoPair d ocs cs = countPossHands TwoPair aphs d ocs cs
    where aphs = [ncs | [v1,v2] <- apvs, ss1 <- apss, ss2 <- apss, let ncs = fromVS [v1] ss1 ++ fromVS [v2] ss2, noBetterHts ncs v1 v2 ss1 ss2]
          noBetterHts ncs v1 v2 ss1 ss2 = noNPlets v1 v2 ss1 ss2 && noStraights cs [v1,v2] && noFlushes' ncs
            -- Implicit Three or Four OfAKinds
          noNPlets v1 v2 ss1 ss2 = all (`notElem` cs) $ makeCs [v1,v1,v2,v2] (concat $ map (allSuits\\) [ss1,ss2])
            -- Implicit Flushes
          noFlushes' ncs = not . any ((>=5) . length) . suitGroups $ union cs ncs

          apvs = combinations 2 allValues
          apss = combinations 2 allSuits


countOnePair d ocs cs = countPossHands OnePair aphs d ocs cs
    where aphs
            | null nPletsVgs = csvshs ++ ncsvshs
            | otherwise      = []
            -- If Card v s is in cs it can be discarded by noFlushes below, which is ok
          csvshs = [[Card v s] | v <- csvs, s <- allSuits, noFlushes cs [s]]
          ncsvshs = [fromVS [v] ss | v <- allValues \\ csvs, ss <- combinations 2 allSuits, noBetterHts cs v ss]

          noBetterHts cs v ss = noStraights cs [v] && noFlushes cs ss

          (okvgs, nPletsVgs) = partition ((==1) . length) $ valueGroups cs
          csvs = cardsValues cs


countHighCard d ocs cs = countPossHands HighCard aphs d ocs cs
    where aphs = group apcs
          apcs
                -- Any Straight Present
            | any (`subsetOf` csvs) stpvs          = []
                -- Any Flush present
            | any ((>4) . length) $ suitGroups cs  = []
                -- Any nPlet present
            | any ((>1) . length) $ valueGroups cs = []
                -- Otherwise just trim the possible Suits and Values sets
            | otherwise = [Card v s | v <- apvs, s <- apss]

          apvs = (\\) allValues $ union straightVs nPletsVs
          apss = allSuits  \\ flushSs

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
countPossHands :: HandType -> [[Card]] -> Deck -> [Card] -> [Card] -> HandTypeCount
countPossHands ht allPossHands d outCs cs
    | csLeft > 0 = HandTypeCount ht possHands countTuples
    | otherwise  = HandTypeCount ht [] []
        where countTuples = map (\l-> (length l, head l)) $ group hProbs
              hProbs = map (handProb d) possHands
              possHands = sortBy ascLength $ filter ((<= csLeft) . length) neededHands
              neededHands = filter (not . null) $ map (\\cs) notOcsHands
              notOcsHands = filter (not . any (`elem` outCs)) allPossHands
              csLeft = 7 - length outCs - length cs


    -- Return the probability of drawing the given CardSet list from the given Deck
    -- EVOLVE THIS INTO USING ALL THE VALUES IN Deck AND CONSTRUCTORS OF CardSet
handProb :: (Fractional a) => Deck -> [Card] -> a
handProb d css = 1 / fromIntegral ((cardsIn d) `choose` (length css))


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
checkAllHtCs :: Deck -> [Card] -> [Card] -> [(HandType, [HandType])]
checkAllHtCs d ocs cs = filter (not . null . snd) . map bad . map getChecks . reverse $ enumFrom HighCard
    where bad (ht,cht) = (ht, nub . sort $ filter (/= ht) cht)
          getChecks ht = (ht, checkHtc ht d ocs cs)

    -- Map each HandType to its counter function
getHtcFunc :: HandType -> (Deck -> [Card] -> [Card] -> HandTypeCount)
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
filterBad :: HandType -> Deck -> [Card] -> [Card] -> [[Card]]
filterBad ht d ocs cs = filter ((/=ht) . checkSingle cs) . completers $ (getHtcFunc ht) d ocs cs
    -- Return all the HandTypes yielded by each completer
checkHtc  ht d ocs cs = map    (checkSingle cs)          . completers $ (getHtcFunc ht) d ocs cs
    -- Return the HandType yielded by a completer
checkSingle cs = hType . bestHandType . union cs

