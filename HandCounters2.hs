---- Poker Analyser Hand Counting Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          1.2 - 11-12/06/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the functions related to counting possible
--          Hands from given conditions.
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

import DataTypes
import GeneralFunctions (choose, combinations, ascLength, subsetOf, listUnZip, noSupersets)

import Data.List (tails, group, groupBy, sort, sortBy, delete, nub, partition, union, (\\))
import Data.Char (toLower)
import qualified Data.Sequence as Seq (replicateM)
import Data.Foldable (toList)
import Data.Function (on)
import Control.Applicative ((<$>), (<*>))

import Control.Parallel.Strategies (using, parList, rdeepseq)


    -- For testing purposes (see Testing Functions)
import HandTypeCheckers



---- 1 - COMPLETE HANDTYPE INSTANCES CALCULATORS -------------------------------

    -- Note: use (map simpleTotProbs) on one of the below functions to get nice
    -- Data.Maps of total probabilities per HandType

    -- Return, in the reasonable order (see below), only the counts of HandTypes
    -- better or equal to the Maybe given one or to the one constituted by cs
countBetterHandTypes :: Maybe HandType -> [Int] -> Deck -> [Card] -> [Card] -> [[HandTypeCount]]
countBetterHandTypes mHt tcns d ocs cs = countHandTypes hts tcns d ocs cs
    where hts = enumFromThenTo RoyalFlush StraightFlush bht
          bht = case mHt of
            Just ht -> ht
            Nothing -> hType $ bestHandType cs

    -- Return all HandTypes' counts in the reasonable order (see below)
countAllHandTypes :: [Int] -> Deck -> [Card] -> [Card] -> [[HandTypeCount]]
countAllHandTypes = countHandTypes (reverse allHandTypes)

    -- Apply the given HandTypes' Instances Calculators to the given cards, and
    -- get the results in the reasonable order: sublists corresponding to each
    -- tcns, containing all the HandTypes' counts (descending).
    -- The order before the listUnzip is counts grouped by HandTypes first:
    -- sublists corresponding to each HandType (descending), containing all the
    -- tcns counts
countHandTypes :: [HandType] -> [Int] -> Deck -> [Card] -> [Card] -> [[HandTypeCount]]
countHandTypes hts tcns d ocs cs = listUnZip countsLists
    where countsLists = map (\f-> f tcns' d ocs cs) countFunctions `using` parList rdeepseq
          countFunctions = map getHtcFunc hts
            -- Having this here avoids generating it for every single count
          tcns' = apprTcns tcns $ length ocs + length cs


    -- Map each HandType to its counter function
getHtcFunc :: HandType -> ([Int] -> Deck -> [Card] -> [Card] -> [HandTypeCount])
getHtcFunc ht = case ht of
    RoyalFlush    -> countRoyalFlush
    StraightFlush -> countStraightFlush
    FourOfAKind   -> countFourOfAKind
    FullHouse     -> countFullHouse
    Flush         -> countFlush
    Straight      -> countStraight
    ThreeOfAKind  -> countThreeOfAKind
    TwoPair       -> countTwoPair
    OnePair       -> countOnePair
    HighCard      -> countHighCard



---- 2 - SINGLE HANDTYPE INSTANCES CALCULATORS ---------------------------------

    -- Each of the following functions returns the list of HandTypeCounts of possible
    -- instances of a specific HandType which can be obtained by completing a
    -- set of n cards, where n is each element of the given list of numbers.

    -- Their input is:
    --              [Int] -> Deck -> [Card] -> [Card] -> [HandTypeCount]
    -- multiple numbers of cards left to draw, the current Deck, the cards which
    -- should not be considered (like the player's if working just on the table)
    -- and the cards in question.

    -- Note: each function avoids counting instances of their HandType which
    -- also qualify as a different one (be it higher or lower)
    -- Also, if an instance of its HandType is already present, each function
    -- returns all the better or equal ones, excluding the ones which are worth
    -- the same but different (i.e. the present instane and all the strictly
    -- better ones; think of Straights of different sets of Suits)

    -- Note: most of these functions break down with unreasonable input (like
    -- lists of identical cards, which are not possible)


countRoyalFlush = countPossHands RoyalFlush aphs
    where aphs = fromSVG allSuits (enumFrom Ten)


countStraightFlush tcns d ocs cs = countPossHands StraightFlush aphs tcns d ocs cs
    where aphs = concat $ map (fromSVG allSuits) apvs
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
    where aphs = case filter ((>1) . length) $ valueDescGroups cs of
            (vcs1@[c,d,e]:vcs2:vcss) -> (:) (vcs1++vcs2) . getAphs $ justBetter vcs1 vcs2
            _ -> getAphs apvs'
          getAphs apvs = [makeCs [v3,v3,v3,v2,v2] (ss3++ss2) | (ss3,ss2) <- apsss, (v3,v2) <- apvs, nF v3 ss3 v2 ss2]
          justBetter vcs1 vcs2 = dropWhile (<= (value $ head vcs1, value $ head vcs2)) apvs'
            -- Ensuring v3 /= v2 prevents FourOfAKinds
          apvs' = [(v3,v2) | v3 <- allValues, v2 <- allValues, v3 /= v2]
          apsss = [(ss3,ss2) | ss3 <- combinations 3 allSuits, ss2 <- combinations 2 allSuits]
          nF v3 ss3 v2 ss2 = noFourOfAKinds v3 ss3 && noFourOfAKinds v2 ss2
          noFourOfAKinds v ss = not . any (`elem` cs) $ makeCs (repeat v) (allSuits \\ ss)


countFlush tcns d ocs cs = countPossHands Flush aphs tcns d ocs cs
    where aphs = case filter ((>=5) . length) $ suitGroups cs of
            [] -> [fromSV [s] vs | vs <- apvs, s <- allSuits]
            fl -> [fromSV [s] vs | vs <- filter better apvs, let s = suit . head $ head fl]
                where better = all (>= (minimum . map value $ head fl))

            -- Remove all FullHouses and FourOfAKinds
          apvs = filter ((>2) . length . group . sort) pvs
            -- Note that each combination below is sorted by ascending Value
          pvs = combinations 5 allValues \\ npvs
          npvs = implStrVs ++ explStrVs
            -- Implicit Straight Values (formed with the addition of the considered Cards)
          implStrVs = concat $ map getVs npCompletersVs
          getVs (origVs,vs) = map (\comVs-> sort (vs ++ comVs)) . combinations (5 - length vs) $ allValues \\ origVs
          npCompletersVs = concat $ map getNpComplVs explStrVs
          getNpComplVs origVs = nub $ map (\vs-> (origVs, origVs \\ vs)) csvsCombs
          csvsCombs = concat $ map (flip combinations csvs) [1..length csvs]
          csvs = cardsValues cs
            -- Explicit Straight Values (has to be in this order to do so efficiently)
          explStrVs = (enumFromTo Two Five ++ [Ace]) : (tail straightValues)


countStraight tcns d ocs cs = countPossHands Straight aphs tcns d ocs cs
    where aphs
                -- Any Straight which is not a StraightFlush present
            | straightButNotFlush = []:phs
            | otherwise           = phs
          straightButNotFlush = not (null strVss) && not sameSuits
            where strVss = filter (`subsetOf` csvs) straightValues
                  sameSuits = any (\s-> any (sameSuit s) strVss) allSuits
                  sameSuit s vs = (fromSV [s] vs) `subsetOf` cs

          phs = filter (\h-> not $ any (`subsetOf` h) npcss) phs'
            -- Remove all indirect StraightFlushes: when a Straight
            -- is acheived, but some adjacent cards create a StraightFlush)
          npcss = map (\\cs) . nub . concat $ map strFluCs cs
          strFluCs (Card v s) = map (fromSV [s]) $ strFluVs v
          strFluVs v = map (delete v) $ filter (v `elem`) straightValues

            -- The cards in cs are already not present in these phs'
          phs' = concat [ncss . (\\csvs) $ head tsvs | tsvs <- init $ tails straightValues, noBetterStraight tsvs]
          noBetterStraight (svs:svss) = not $ any (`subsetOf` pBetterSVs) svss
            where pBetterSVs = csvs ++ (svs\\csvs)
          ncss vs = [makeCs vs ss | ss <- apsss !! length vs, noImplicitFlush ss]
          noImplicitFlush = all ((<5) . length) . group . sort . (csss++)
          csss = map suit cs
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
            | otherwise = case bestNplets of
                [[c1,c2],[c3,c4]]:_ -> eqOrBetter (value c3) phs
                _                   -> phs
          bestNplets = groupBy ((==) `on` length) $ valueDescGroups cs
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
            | any (`subsetOf` csvs) straightValues          = []
            | null nPletsVgs                                = csvshs ++ ncsvshs
            | length (head bNPls) == 2 && length bNPls == 1 = bNPls
            | otherwise                                     = []
          csvshs = [[Card v s] | v <- csvs, s <- pss v, noFlushes cs [s]]
          pss v = (\\) allSuits $ map suit $ filter ((==v) . value) cs
          ncsvshs = [fromVS [v] ss | v <- allValues \\ csvs, ss <- combinations 2 allSuits, noBetterHts cs v ss]

          noBetterHts cs v ss = noStraights cs [v] && noFlushes cs ss

            -- Best NPlets
          bNPls = head $ groupBy ((==) `on` length) nPletsVgs
          nPletsVgs = filter ((>1) . length) $ valueDescGroups cs
          csvs = cardsValues cs


countHighCard tcns d ocs cs = countPossHands HighCard aphs tcns d ocs cs
    where aphs = map (:[]) apcs
          apcs
                -- Any Straight Present
            | any (`subsetOf` csvs) stpvs          = []
                -- Any Flush present
            | any ((>4) . length) $ suitGroups cs  = []
                -- Any nPlet present
            | any ((>1) . length) $ valueGroups cs = []
            | null cs   = allCards
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



---- 3 - GENERAL FUNCTIONS -----------------------------------------------------

    -- Possible Hands narrowing down process common to all count functions
countPossHands :: HandType -> [[Card]] -> [Int] -> Deck -> [Card] -> [Card] -> [HandTypeCount]
countPossHands ht allPossHands tcns d outCs cs = map finalCount tcns'
    where tcns' = apprTcns tcns alreadyDrawn
          finalCount tcn = HandTypeCount ht possHands tcn countTuples
            where leftToDraw = tcn - alreadyDrawn
                  countTuples = map ((,) <$> length <*> handProb leftToDraw d . head) possHandsGs
                  possHandsGs = groupBy ((==) `on` length) $ sortBy ascLength possHands
                  possHands = filter ((<= leftToDraw) . length) neededHands
          neededHands = map (\\cs) notOcsHands
          notOcsHands = filter (not . any (`elem` outCs)) allPossHands
          alreadyDrawn = length outCs + length cs


    -- Return the appropriate Target Card NumberS for a given number of cards
    -- which are already in play
apprTcns :: [Int] -> Int -> [Int]
apprTcns tcns alreadyDrawn
    | null tcns = dropWhile (< alreadyDrawn) [2,5,6,7]
    | otherwise = tcns
    -- Same thing but given the current round stage
stageTcns :: Char -> [Int]
stageTcns stage = case toLower stage of
   's' -> [2,5,6,7] -- RoundStart
   'f' ->   [5,6,7] -- Flop
   't' ->     [6,7] -- Turn
   'r' ->       [7] -- River


    -- Return the probability of drawing the given Cards from the given Deck in
    -- the given number of draws
    -- EVOLVE THIS INTO USING ALL THE VALUES IN Deck
handProb :: (Fractional a) => Int -> Deck -> [Card] -> a
handProb ltd d cs
    | null cs   = fromIntegral 1
    | otherwise = ((/) `on` fromIntegral) drawsWithCs allPossDraws
        where -- The ok draws are the ones which contain the h cards and any of the
              -- possible sets of (k-h) cards wich can be made from the remaining (n-h) cards
              drawsWithCs  = n - h `choose` k - h
              allPossDraws = n     `choose` k
              -- h <= k <= n
              n = cardsIn d
              k = ltd
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



---- 4 - TESTING FUNCTIONS -----------------------------------------------------

    -- Check (as described in checkHtCounts) only the appropriate HandTypes (eq
    -- or better than what cs constitute)
checkBetterHandTypes :: [Int] -> Deck -> [Card] -> [Card] -> [(HandType, [[HandType]])]
checkBetterHandTypes tcns d ocs cs = checkHtCounts hts tcns d ocs cs
    where hts = enumFromThenTo RoyalFlush StraightFlush . hType $ bestHandType cs


    -- Check (as described in checkHtCounts) all the HandTypes
checkAllHandTypes :: [Int] -> Deck -> [Card] -> [Card] -> [(HandType, [[HandType]])]
checkAllHandTypes = checkHtCounts (reverse allHandTypes)


    -- Test whether any of the count functions of the given HandTypes yields a
    -- HandType which is not its own. In particular, lookout for ones higher
    -- than it. Also, count the instances of each
checkHtCounts :: [HandType] -> [Int] -> Deck -> [Card] -> [Card] -> [(HandType, [[HandType]])]
checkHtCounts hts tcns d ocs cs = list `using` parList rdeepseq
    where list = filter (not . null . snd) $ map (notNulls . bad . getChecks) hts
          notNulls (ht,badChts) = (ht, filter (not . null) badChts)
          bad (ht,chts) = (ht, map (nub . sort . filter (/= ht)) chts)
          getChecks ht = (ht, checkHtc ht tcns d ocs cs)


    -- Return all the completers which yield different HandTypes than the required one
filterBad :: HandType -> [Int] -> Deck -> [Card] -> [Card] -> [[[Card]]]
filterBad ht tcns d ocs cs = map (filter ((/=ht) . checkSingle cs) . completers) $ getHtcFunc ht tcns d ocs cs
    -- Return all the HandTypes yielded by each completer
checkHtc :: HandType -> [Int] -> Deck -> [Card] -> [Card] -> [[HandType]]
checkHtc  ht tcns d ocs cs = map (map    (checkSingle cs)          . completers) $ getHtcFunc ht tcns d ocs cs
    -- Return the HandType yielded by a completer
checkSingle cs = hType . bestHandType . union cs

