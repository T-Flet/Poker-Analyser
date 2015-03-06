---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.7 - 04-05/03/2015
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
--       3 - Other Functions
--

---- 0 - TO DO and TO CONSIDER -------------------------------------------------

-- PERHAPS GROUP straightProb AND highCardProb TOGETHER

-- INTRODUCE quality FIELD IN Prob, REPRESENTING HOW GOOD A HandType IT IS
-- AMONG ALL POSSIBLE SAME HandTypes
    -- PERHAPS IT SHOULD BE IN Hand INSTEAD?
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


---- 00 - TESTING DATA ---------------------------------------------------------

-- let a = [Card Spades King, Card Hearts Queen, Card Clubs Jack]
-- let b = [Prob HighCard 1 [], Prob FullHouse 0.3 [Left Ace], Prob Straight 0.8 [Right Diamonds]]
-- let h = probsToHand (sort a) (reverse $ sort b)



---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sort)


data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Suit = Spades | Clubs | Diamonds | Hearts
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card {suit :: Suit, value :: Value}
                deriving (Eq, Ord, Show)
instance Enum Card where
    toEnum i   = Card (toEnum (i`div`13) :: Suit) (toEnum (i`mod`13) :: Value)
    fromEnum c = (fromEnum $ value c) + ((*13) . fromEnum $ suit c)
    enumFrom c = dropWhile (<c) [Card s v |
        s <- enumFrom $ (minBound :: Suit), v <- enumFrom $ (minBound :: Value)]
-- All the other Enum functions are automatically defined from toEnum and fromEnum

data Hand = Hand {hKind :: HandType, cards :: [Card]}
                deriving (Eq, Ord, Show)

data Prob = Prob {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)



---- 2 - MAIN FUNCTIONS --------------------------------------------------------

main = do
    line <- getLine
    let myHand = read line :: (Suit, Value)
    print $ Card (fst myHand) (snd myHand)



---- 3 - OTHER FUNCTIONS -------------------------------------------------------

-- bestHand :: [Card] -> Hand
-- bestHand cs = probsToHand scs . foldr addCard noProbs scs
--     where scs = sort cs
--           addCard = changeProbs scs

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
-- changeProbs :: [Card] -> Card -> [Prob] -> [Prob]
-- changeProbs scs c [rF, sF, fK, fH, fl, st, tK, tP, oP, hC] =
--                 [rF', sF', fK', fH', fl', st', tK', tP', oP', hC']
--     where (rF', sF', fl')           = flushesProbs  (rF, sF, fl) scs c
--           (fK', fH', tK', tP', oP') = nOfAKindProbs (fK, fH, tz, tP, oP) scs c
--           st'                       = straightProb  st scs c
--           hC'                       = highCardProb  hC scs c


--aimingFor :: [Prob] -> Prob
-- Perhaps this should just be bestChance

    -- Returns Probabilities of RoyalFlush, StraightFlush and Flush
-- flushesProbs :: (Prob,Prob,Prob) -> [Card] -> Card -> (Prob,Prob,Prob)
-- flushesProbs (rF, sF, fl) scs c



    -- Returns Probabilities of FourOfAKind, FullHouse, ThreeOfAKind, TwoPair and OnePair
-- nOfAKindProbs :: (Prob,Prob,Prob,Prob,Prob) -> [Card] -> Card -> (Prob,Prob,Prob,Prob,Prob)
-- nOfAKindProbs (fK, fH, tz, tP, oP) scs c =



    -- Returns the Probability of a Straight
-- straightProb :: Prob -> [Card] -> Card -> Prob
-- straightProb st scs c =



    -- Returns the Probability of a HighCard
-- highCardProb :: Prob -> [Card] -> Card -> Prob
-- highCardProb hC scs c = Prob HighCard 1 []


type Qual = Int
    -- Returns the Quality of a HighCard
highCardQual :: Hand -> Qual
highCardQual h = fromEnum . head $ h

