---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.14 - 17-18/03/2015
--
--      Description:
--          Poker analysing shell.
--          Inupts:
--              Player's cards, table cards (later on: player's fiches and even
--              later other players' as well).
--              Possible user request for (his or other's) probability of a
--              specific hand.
--          Outputs:
--              Probability of player's or others' specific hand.
--              Specific request response.
--              (Later: suggested bet)
--
--   Sections:
--       0 - TO DO and TO CONSIDER
--       00- Testing Data
--       1 - Imports and Type declarations
--       2 - Main Functions
--       3 - Shell Functions
--       4 - Other Functions
--

---- 0 - TO DO and TO CONSIDER -------------------------------------------------

-- GOOD SKETCH OF PROBABILITIES: https://en.wikipedia.org/wiki/Poker_probability_(Texas_hold_%27em)
    -- LOOK AT HAND DOMINATION

-- COULD JUST BRUTE FORCE ALL POSSIBLE HANDS, SORT THEM, RANK THEM AND SAVE THE
-- RESULT AS A DATA STRUCTURE TO IMPORT.
    -- OR DO IT SLIGHTLY MORE SMARTLY, BY GROUPING THEM.
    --
    -- START BY DOING IT FOR STARTING PAIRS. VERY IMPORTANT. DO IT!!!!!!!!

-- IN GENERAL: NEED TO IMPLEMENT CONDITIONAL PROBABILITIES (A|B) IN ORDER TO
-- CATER FOR THE FACT THAT SOME CARDS THAT ARE NEEDED FOR A HAND COULD HAVE
-- ALREADY BEEN EXTRACTED AND BE IN OTHER PLAYERS' HANDS.

-- EVEN IF THE INCREMENTAL PROBABILITY ENDS UP NOT BEING IMPLEMENTED, MAKE IT
-- SO THAT EVERYTHING IS FIRST CALCULATED FOR THE TABLE SO THAT PROBABILITIES
-- FOR ALL PLAYERS ARE KNOWN, AND THEN APPLY IT TO THE SPECIFIC PLAYER'S HAND

-- SHOULD THE NUMBER OF PLAYERS BE A GLOBAL VARIABLE? OR SHOULD IT BE PASSED
-- AROUND?

-- ADD AN "OR" CLAUSE IN THE need FIELD OF Prob

-- CONSIDER REMOVING THE HandType VALUE FROM Hand AND Prob, AND JUST MAKE
-- Data.MapS (DICTIONARIES) OF ( (HandType,Prob) AND (HandType,Hand) ) OR
-- (HandType,Either Prob Hand)

-- REALLY THINK ABOUT INCREMENTAL VS COMPREHENSIVE PROBABILITY DETERMINATION

-- PERHAPS GROUP straightProb AND highCardProb TOGETHER

-- INTRODUCE better (OR SOMETHING SIMILAR) FIELD IN Prob, REPRESENTING THE
-- SMALLEST CARD REQUIRED TO GET A BETTER HAND THAN THE PRESENT.
-- IT IS DIFFERENT FROM THE need FIELD, AND IT SHOULD WORK WITH IT

-- INTRODUCE quality FIELD IN Prob, REPRESENTING HOW GOOD A HandType IT IS
-- AMONG ALL POSSIBLE SAME HandTypes
    -- PERHAPS IT SHOULD BE IN Hand INSTEAD?
        -- AND A SAME TYPE FIELD SUCH AS "BEST POSSIBLE" COULD BE IN Prob
    -- REGARDLESS:
        -- NEED A SET OF BIJECTIONS (ONE PER HandType)
        -- F: HandType x |-> [0..numberOfAllPossibleShuchHandTypes-1]
            -- OR PERHAPS FROM 1 TO (NOT -1)
            -- E.G. : HighCard -> [0..13-1]

-- FUNCTION whatProb WHICH IS GIVEN THE PRESENT CARDS AND STUFF LIKE
-- Either Value Suit OR [Card] AND RETURNS THE PROBABILITY OF GETTING SUCH A
-- SET FROM THE PRESENT ONES
    -- PERHAPS THE need FIELD IN Prob SHOULD BE OF THE TYPE OF THAT STUFF
    -- OR Prob SHOULD ALSO HAVE A cards FIELD LIKE Hand.

-- STRUCTURING CAN BE THE FOLLOWING:
--  ONE FUNCTION TAKES THE TABLE AND RETURNS Prob OF ALL HandtypeS;
--  THEN THE PLAYER'S HAND IS TAKEN IN AND MAPPED OVER THE PROBABILITIES;
--  EITHER STOP AT THE FIRST 100% OR DO THEM ALL (OR BE LAZY AFTER THE FIRST ONE)

-- MAKE ALL THESE FUNCTIONS ASSUME THE PREVIOUS ONE HAS RUN?
-- MAKE THEM WORK BY COUNTING THE CARDS THAT ARE NOT "OUT"?
-- AND PERHAPS ALL POSSIBLE OTHER PLAYERS' HANDS?


-- DISTINCTIVE PROPERTY OF THIS PROJECT WILL BE THE QUALITY OF HANDS:
-- FROM THE SET OF ALL POSSIBLE 5 CARDS (52C5), THE 10 PARTITIONS IN DIFFERENT
-- HANDTYPES WILL BE IDENTIFIED AND INDIVIDUALLY SORTED BY CREATING BIJECTIONS
-- (IN FACT SIMILAR TO A fromEnum) FROM EACH OF THEM TO INTEGERS FROM 0 TO THE
-- NUMBER OF ALL POSSIBLE SUCH HANDS IN ORDER.


-- NOTE: THERE WILL BE MANY HANDS WHICH WILL BE IN MORE THAN ONE HANDTYPE


-- NOTE: THE ONLY HANDS WHICH WILL NEED TO BE CALCULATED EACH TIME ARE
-- THE ONES THAT WOULD BEAT THE PLAYER'S OWN.


-- NOTE: THERE CAN BE A CRUDER VERSION OF QUALITY OF HANDS: BY NOT SORTING ALL
-- SINGLE POSSIBLE ONES BUT BY GROUPING THEM BY KINDS.
-- E.G. SORT FULLHOUSES BY WHAT THE TRIS IS OF AND WHAT THE PAIR IS OF.



---- 00 - TESTING DATA ---------------------------------------------------------

-- let a = [Card King Spades, Card Queen Hearts, Card Jack Clubs]
-- let b = [Prob HighCard 1 [], Prob FullHouse 0.3 [Left Ace], Prob Straight 0.8 [Right Diamonds]]
-- let h = probsToHand (sort a) (reverse $ sort b)



---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sort, sortBy, groupBy)
import Data.Char (toLower)
import Data.Map (lookup)


data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Suit = Spades | Clubs | Diamonds | Hearts
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card {value :: Value, suit :: Suit}
                deriving (Eq, Ord, Show)
instance Enum Card where
    toEnum i   = Card (toEnum (i`mod`13) :: Value) (toEnum (i`div`13) :: Suit)
    fromEnum c = (fromEnum $ value c) + ((*13) . fromEnum $ suit c)
        -- This enumFrom is in deck order (suit first), while actual card
        -- comparison is by value first
    enumFrom c = dropWhile (<c) [Card v s |
        s <- enumFrom $ (minBound :: Suit), v <- enumFrom $ (minBound :: Value)]
-- All the other Enum functions are automatically derived from toEnum and fromEnum

data Hand = Hand {hKind :: HandType, cards :: [Card]}
                deriving (Eq, Ord, Show)

data Prob = Prob {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)


data Player = Player {num :: Int, balance :: Int}
    -- EVENTUALLY INTRODUCE STATISTICS TRACKING IN HERE

data Action = SetPlayers | SetDealer | Start | Discard | Flop | Turn | River | Bet | Raise | Fold
    -- PERHAPS SPLIT INTO Stage AND Action, WHERE ACTION HAS PLAYER AND AMOUNT FIELDS

data Frame = Frame {action :: Action, playersNum :: Int, dealer :: Int,
                    cardsInDeck :: Int, table :: [Card], myCards :: [Card],
                    players :: [Player]}
                    -- The actual player is the first (0) in players list

type State = [Frame]



---- 2 - MAIN FUNCTIONS --------------------------------------------------------

main = do
    line <- getLine
    let myHand = read line :: (Value, Suit)
    print $ Card (fst myHand) (snd myHand)

-- SOME LOOP WHICH TAKES A LINE AND FEEDS IT TO A FUNCTION WHICH TAKES A STATE
-- AND RETURNS A STRING TO PRINT AND A NEW STATE

    -- THIS SECTION IS JUST FOR TESTING
--    line <- getLine
--    print $ gameShell line
--gameShell :: String -> String
--gameShell cmd = case cmd of
--        -- Set players number
--    ('p':'s':' ':n:_) -> "Players number set to " ++ [n]
--    _                 -> "OTHERWISE"


-- THE PREVIOUSLY MENTIONED FUNCTION
-- PERHAPS LATER MADE AS REGEXES (IF MAKING IT SAFER IS DIFFICULT IN THIS WAY)
--gameShell :: State -> String -> (State,String)
--gameShell s cmd = case cmd of
--        -- Set players number
--    ('p':'s':' ':n:_) ->
--                 (setPlayers s (read n :: Int),
--                  "Players number set to " ++ n)
--        -- Player x is dealer (the actual player is the first in whichever direction)
--    ('p':x:'d':_) ->
--                 (setDealer s (read x :: Int),
--                  "Player " ++ x ++ " is dealer")
--        -- Back one action
--    ('b':_) ->
--                 (tail s,
--                  "Revoked last action: " ++ (action $ head s))
--        -- Set initial hand
--    ('h':' ':v1:s1:' ':v2:s2:_) ->
--                 let cs = map toCard [[v1,s1],[v2,s2]]
--                 in (startHand s cs,
--                     "Starting hand added: " ++ (show cs))
--        -- Discard n cards (it can happen)
--    ('d':' ':n:_) ->
--                 (discard s (read n :: Int),
--                  "Discarded " ++ n ++ " cards")
--        -- Flop
--    ('c':' ':v1:s1:' ':v2:s2:' ':v3:s3:_) ->
--                 let cs = map toCard [[v1,s1],[v2,s2],[v3,s3]]
--                 in (addCards s cs,
--                     "Flop added: " ++ (show cs))
--        -- Add card
--    ('c':' ':v1:s1:_) ->
--                 let c = map toCard [[v1,s1]]
--                 in (addCards s cs,
--                     "Card added: " ++ (show c))
--        -- Player x Folds
--    ('p':x:'f':_) ->
--                 (plFolds s (read x :: Int),
--                  "Player " ++ x ++ " folded")
--        -- Player x Bets (or Raises, but reporting the bet) by amount
--    ('p':x:'b':a:_) ->
--                 (plBets s (read x :: Int) (read a :: Int),
--                  "Player " ++ x ++ " bet " ++ a)
--        -- Player x Raises by amount
--    ('p':x:'r':a_) ->
--                 (plRaises s (read x :: Int) (read a :: Int),
--                  "Player " ++ x ++ " raised " ++ a)
--        -- Otherwise: not a recognised command
--    _            -> (s, "Command not recognised")



---- 3 - SHELL FUNCTIONS -------------------------------------------------------

setPlayers :: State -> Int -> State
setPlayers s@(f:fs) n = nf:s
    where nf = Frame SetPlayers n (dealer f) (cardsInDeck f) (table f) (myCards f) (players f)


setDealer :: State -> Int -> State
setDealer s@(f:fs) p = nf:s
    where nf = Frame SetDealer (playersNum f) p (cardsInDeck f) (table f) (myCards f) (players f)


startHand :: State -> [Card] -> State
startHand s@(f:fs) cs = nf:s
    where nf = Frame Start (playersNum f) (dealer f) ndcs (table f) cs (players f)
          ndcs = (cardsInDeck f) - 2 * (playersNum f)


discard :: State -> Int -> State
discard s@(f:fs) n = nf:s
    where nf = Frame Discard (playersNum f) (dealer f) ndcs (table f) (myCards f) (players f)
          ndcs = (cardsInDeck f) - n


    -- Function to maybe get a card from a pair of value and suit characters
toCard :: [Char] -> Maybe Card
toCard uVS
    | vsMatch = Just $ Card val sui
    | otherwise = Nothing
        where vsMatch = v `elem` "234567891jqka" && s `elem` "scdh"
              [v,s] = map toLower uVS
              val = case v of
                    '2' -> Two  ; '3' -> Three ; '4' -> Four  ; '5' -> Five ;
                    '6' -> Six  ; '7' -> Seven ; '8' -> Eight ; '9' -> Nine ;
                    '1' -> Ten  ; 'j' -> Jack  ; 'q' -> Queen ; 'k' -> King ;
                    'a' -> Ace
              sui = case s of
                    's' -> Spades   ; 'c' -> Clubs ;
                    'd' -> Diamonds ; 'h' -> Hearts


    -- FIND A WAY TO STREAMLINE THOSE FUNCTIONS
    -- ALSO, THINK OF A WAY TO TIE THIS TO THE ACTUAL DATA TYPE
    -- ALSO, THINK OF POLYMORPHISM AND THE FACT THAT SOME THING SHOULD, PERHAPS NOT BE "READ" BUT JUST PASSED
--newFrame :: State -> M.Map String String -> State
--newFrame s@(f:fs) fieldMap = Frame act plN dea dCN tab mCs pls
--    where act = maybe (action f) (\x -> read x :: Action) $ M.lookup "action" fieldMap
--          plN = maybe (playersNum f) (\x -> read x :: Int) $ M.lookup "playersNum" fieldMap
--          dea = maybe (dealer f) (\x -> read x :: Int) $ M.lookup "dealer" fieldMap
--          dCN = maybe (cardsInDeck f) (\x -> read x :: Int) $ M.lookup "cardsInDeck" fieldMap
--          tab = maybe (table f) (\x -> read x :: [Card]) $ M.lookup "table" fieldMap
--          mCs = maybe (myCards f) (\x -> read x :: [Card]) $ M.lookup "myCards" fieldMap
--          pls = maybe (players f) (\x -> read x :: [Player]) $ M.lookup "players" fieldMap
--
--          func (field, reader, key) =



---- 4 - OTHER FUNCTIONS -------------------------------------------------------

    -- Classic mathematical function
choose :: Int -> Int -> Int
n `choose` k = product [k+1..n] `div` product [1..n-k]


    -- Sort cards by suit first (as in deck order)
sortBySuit :: [Card] -> [Card]
sortBySuit cs = sortBy cmpSui cs
    where cmpSui c1 c2
            | s1 >  s2 = GT
            | s1 == s2 = compare v1 v2
            | s1 <  s2 = LT
                where v1 = value c1
                      v2 = value c2
                      s1 = suit  c1
                      s2 = suit  c2


    -- Return the hand the player actually has
-- bestHand :: [Card] -> Hand
-- bestHand cs = probsToHand scs . snd $ foldr addCard noProbs scs
--     where scs = sort cs


    -- Assumes Probabilities are sorted by descending HandType
    -- and that at least one has a 100% chance
probsToHand :: [Card] -> [Prob] -> Hand
probsToHand scs prs = Hand hT scs
    where hT = pKind . head . dropWhile ((/= 1) . chance) $ prs


    -- List of "empty" probabilities in descending HandType
noProbs :: [Prob]
noProbs = map (\hT-> Prob hT 0 []) . reverse . enumFrom $ (minBound :: HandType)


    -- Change the existing probabilities on existing list of cards by taking a
    -- new card into consideration
-- addCard :: Card -> [Prob] -> [Prob]
-- addCard c (scs, [rF, sF, fK, fH, fl, st, tK, tP, oP, hC]) =
--              (c:scs, [rF', sF', fK', fH', fl', st', tK', tP', oP', hC'])
--     where (rF', sF', fl')           = flushesProbs  (rF, sF, fl) scs c
--           (fK', fH', tK', tP', oP') = nOfAKindProbs (fK, fH, tz, tP, oP) scs c
--           st'                       = straightProb  st scs c
--           hC'                       = highCardProb  hC scs c


--aimingFor :: [Prob] -> Prob
-- Perhaps this should just be bestChance

    -- Returns Probabilities of RoyalFlush, StraightFlush and Flush
-- flushesProbs :: (Prob,Prob,Prob) -> [Card] -> Card -> (Prob,Prob,Prob)
-- flushesProbs (rF, sF, fl) scs c = (rF', sF', fl')
--    where rF' =
--          sF' =
--          fl' = Prob Flush flChance flNeed
--
--          flChance = sum $ map check required
--              where n  = 52 - 2*(nPlayers - 1) - (length scs + 1)
--                    check x
--                      | x > left  = 0
--                      | otherwise = (1/) . choose n x
--              ---- NEED TO CARRY AROUND OR, IN GENERAL, KNOW WHAT SUIT IS BEING CONSIDERED.
--              ---- ALSO, NEED TO TAKE INTO ACCOUNT THE CONDITIONAL (A|B) PROBABILITY
--              ---- OF EXTRACTING THE NEEDED NUMBER OF CARDS OF THE SPECIFIC SUITS GIVEN
--              ---- THAT left CARDS WILL BE/HAVE BEEN EXTRACTED. (THE choose RIGHT
--              ---- BEFORE THESE COMMENTS SHOULD BE SUCH A CONDITIONAL ONE).
--
--              -- Cards left to extract in Texas Hold'em (one card is 'c')
--          left = 6 - length scs
--              ---- WRONG: NOT CONSIDERING DISCARDED CARDS, DIFFERENT AT EACH TURN.
--
--              -- Number of required cards of the same suits
--          required = map (5-) $ map length suitGroups
--
--              -- Split cards by suits and order them by size of sets
--          suitGroups = sort . groupBy eqSuit $ sortBySuit scs
--          eqSuit c1 c2 = (suit c1) == (suit c2)



    -- Returns Probabilities of FourOfAKind, FullHouse, ThreeOfAKind, TwoPair and OnePair
-- nOfAKindProbs :: (Prob,Prob,Prob,Prob,Prob) -> [Card] -> Card -> (Prob,Prob,Prob,Prob,Prob)
-- nOfAKindProbs (fK, fH, tz, tP, oP) scs c = (fK', fH', tK', tP', oP')
--    where fK' =
--          fH' =
--          tK' =
--          tP' =
--          oP' =



    -- Returns the Probability of a Straight
-- straightProb :: Prob -> [Card] -> Card -> Prob
-- straightProb st scs c =



    -- Returns the Probability of a HighCard
-- highCardProb :: Prob -> [Card] -> Card -> Prob
-- highCardProb hC scs c = Prob HighCard 1 []


type Qual = Int
    -- Returns the Quality of a HighCard
highCardQual :: Hand -> Qual
highCardQual h = fromEnum . head . cards $ h

