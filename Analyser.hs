---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.15 - 18-19/03/2015
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

-- ADD FRAME NUMBER FIELD

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
-- let b = [Card Three Spades, Card Seven Hearts, Card Eight Diamonds]
-- let prs = [Prob HighCard 1 [], Prob FullHouse 0.3 [Left Ace], Prob Straight 0.8 [Right Diamonds]]
-- let h = probsToHand (sort a) (reverse $ sort b)

-- let pls = [Player 2 300]
-- let fr = Frame Discard 4 2 40 a b pls
-- let fl = [("action", FA Fold), ("dealer", FI 3)]
-- let ss = newFrame [fr] fl



---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sort, sortBy, groupBy)
import Data.Char (toLower)
import qualified Data.Map as M (Map, lookup, fromList)


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


data Player = Player {num :: Int, balance :: Int, onPlate :: Int, status :: Action}
                deriving (Eq, Ord, Show)
    -- EVENTUALLY INTRODUCE STATISTICS TRACKING IN HERE

data Action = SetPlayers | SetDealer | Discard | RoundEnd
                | Start | Flop | Turn | River | GameEnd
                | Check | Bet | Raise | Fold | Out
                deriving (Eq, Ord, Show)
    -- PERHAPS SPLIT INTO Stage AND Action, WHERE ACTION HAS PLAYER AND AMOUNT FIELDS

data Frame = Frame {action :: Action, playersNum :: Int, dealer :: Int,
                    cardsInDeck :: Int, table :: [Card], myCards :: [Card],
                    plate :: Int, players :: [Player]}
                deriving (Eq, Show)
                    -- The actual player is the first (0) in players list

data FrameField = FA Action | FI Int | FC [Card] | FP [Player]
                deriving (Eq, Show)
toA (FA x) = x
toI (FI x) = x
toC (FC x) = x
toP (FP x) = x

type State = [Frame]



---- 2 - MAIN FUNCTIONS --------------------------------------------------------

main = do
    line <- getLine
    print . snd $ gameShell [] line

-- SOME LOOP WHICH TAKES A LINE AND FEEDS IT TO A FUNCTION WHICH TAKES A STATE
-- AND RETURNS A STRING TO PRINT AND A NEW STATE



    -- PERHAPS LATER MAKE WITH REGEXES (IF MAKING IT SAFER IS DIFFICULT IN THIS WAY)
    -- TIDY UP:
    --  COULD INCLUDE THE TUPLE IN THE SINGLE FUNCTIONS INSTEAD OF HERE
    --  COULD USE A LET OR WHERE CLAUSE TO MAKE THE CHARS STRINGS INSTEAD OF DOING, FOR EXAMPLE, [x]
gameShell :: State -> String -> (State,String)
gameShell s cmd = case cmd of
    -- Player related commands start with p
        -- Set players number
    ('p':'n':' ':n:_) ->
                (setPlayers s (read [n] :: Int),
                    "Players number set to " ++ [n])
        -- Player x is dealer (the actual player is the first in whichever direction)
    ('p':'d':' ':x:_) ->
                (setDealer s (read [x] :: Int),
                    "Player " ++ [x] ++ " is dealer")
        -- Player x Folds
    ('p':'f':x:_) ->
                (plFolds s (read [x] :: Int),
                    "Player " ++ [x] ++ " folded")
        -- Player x Bets (or Raises, but reporting the bet) by amount
    ('p':'b':x:' ':a:_) ->
                (plBets s Bet (read [x] :: Int) (read [a] :: Int),
                    "Player " ++ [x] ++ " bet " ++ [a])
        -- Player x Raises by amount
    ('p':'r':x:' ':a:_) ->
                (plBets s Raise (read [x] :: Int) (read [a] :: Int),
                    "Player " ++ [x] ++ " raised " ++ [a])

        -- Back one action
    ('b':_) ->
                (tail s,
                    "Revoked last action: " ++ (show . action $ head s))

    -- Card related commands start with p
        -- Discard n cards (it can happen)
    ('c':'d':' ':n:_) ->
                (discard s (read [n] :: Int),
                    "Discarded " ++ [n] ++ " cards")
        -- Set initial hand
    ('c':'i':' ':v1:s1:' ':v2:s2:_) ->
                cardHandler s Start [[v1,s1],[v2,s2]]
        -- Flop
    ('c':'f':' ':v1:s1:' ':v2:s2:' ':v3:s3:_) ->
                cardHandler s Flop  [[v1,s1],[v2,s2],[v3,s3]]
        -- Turn
    ('c':'t':' ':v1:s1:_) ->
                cardHandler s Turn  [[v1,s1]]
        -- River
    ('c':'r':' ':v1:s1:_) ->
                cardHandler s River [[v1,s1]]

        -- Otherwise: not a recognised command
    _            -> (s, "Command not recognised")



---- 3 - SHELL FUNCTIONS -------------------------------------------------------

    -- Set the number of players
setPlayers :: State -> Int -> State
setPlayers s n = newFrame s [("action", FA SetPlayers), ("playersNum", FI n)]


    -- Set the player x (x after the actual player) to be the dealer
setDealer :: State -> Int -> State
setDealer s p = newFrame s [("action", FA SetDealer), ("dealer", FI p)]


    -- Give out two cards per player
startHand :: State -> [Card] -> State
startHand s@(f:_) cs = newFrame s [("action", FA Start), ("myCards", FC cs), ("cardsInDeck", FI ndcs)]
    where ndcs = (cardsInDeck f) - 2 * (playersNum f)


    -- Discard n cards (for some reason)
discard :: State -> Int -> State
discard s@(f:_) n = newFrame s [("action", FA Discard), ("cardsInDeck", FI ndcs)]
    where ndcs = (cardsInDeck f) - n


    -- ADD THE FACT THAT CARDS ARE DISCARDED!!!
    -- Add some cards to the table (Flop, Turn, River)
addCards :: State -> Action -> [Card] -> State
addCards s@(f:_) act cs = newFrame s [("action", FA act), ("cardsInDeck", FI ndcs), ("table", FC ntab)]
    where ndcs = (cardsInDeck f) - (length cs)
          ntab = cs ++ (table f)


    -- Player x Folds
plFolds :: State -> Int -> State
plFolds s@(f:_) x = newFrame s [("action", FA Fold), ("players", FP npls)]
    where npls = map plStatus (players f)
          plStatus pl
            | num pl == x = Player x (balance pl) (onPlate pl) Fold
            | otherwise   = pl


    -- INTRODUCE NEGATIVE BALANCE CHECKS SOMEWHERE
    -- ALSO, CHECK THAT A Bet IS >= THE PREVIOUS ONE
    -- Player x bets or raises by amount a
plBets :: State -> Action -> Int -> Int -> State
plBets s@(f:_) act x a = newFrame s [("action", FA act), ("players", FP npls)]
    where npls = map plStatus (players f)
          plStatus pl
            | num pl == x && act == Bet   = Player x nBal nPlt act
            | num pl == x && act == Raise = Player x (nBal-pPPlt) (nPlt+pPPlt) act
            | otherwise                   = pl
                where nBal = ((balance pl) - a)
                      nPlt = ((onPlate pl) + a)

                      pPPlt = onPlate . head . filter ((== (x-1) `mod` (playersNum f)) . num) $ players f


    -- Add a new Frame to the State by providing only the fields which change
    -- with respect to the previous one
newFrame :: State -> [(String, FrameField)] -> State
newFrame s@(f:fs) fieldList = (Frame act plN dea dCN tab mCs plt pls):s
    where act = newField action      toA "action"
          plN = newField playersNum  toI "playersNum"
          dea = newField dealer      toI "dealer"
          dCN = newField cardsInDeck toI "cardsInDeck"
          tab = newField table       toC "table"
          mCs = newField myCards     toC "myCards"
          plt = newField plate       toI "plate"
          pls = newField players     toP "players"

          newField field extractor key =
                maybe (field f) extractor . M.lookup key $ M.fromList fieldList


    -- Take cards in shorthand as input, and if correct, execute a Start, Flop,
    -- Turn or River
cardHandler :: State -> Action -> [String] -> (State,String)
cardHandler s act sCs = maybe (s, "Cards have been mistyped") actFunc $ mCs
    where mCs = sequence $ map toCard sCs
          actFunc = case act of
                        Start -> (\cs -> (startHand s cs,
                                    "Starting hand added: " ++ (show cs)) )
                        Flop  -> (\cs -> (addCards s Flop cs,
                                    "Flop added: " ++ (show cs)) )
                        Turn  -> (\cs -> (addCards s Turn cs,
                                    "Card added: " ++ (show cs)) )
                        River -> (\cs -> (addCards s River cs,
                                    "Card added: " ++ (show cs)) )


    -- Maybe get a card from a pair of value and suit characters
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

