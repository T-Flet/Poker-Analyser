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
    -- PERHAPS IT SHOULD BE IN HandType INSTEAD?

-- FUNCTION whatProb WHICH IS GIVEN THE PRESENT CARDS AND STUFF LIKE
-- Either Value Suit OR [Card] AND RETURNS THE PROBABILITY OF GETTING SUCH A
-- SET FROM THE PRESENT ONES
    -- PERHAPS THE need FIELD IN Prob SHOULD BE OF THE TYPE OF THAT STUFF
    -- OR Prob SHOULD ALSO HAVE A cards FIELD LIKE Hand.

--IN WHICH CASE (OR NOT) MAKE IT A FUNCTOR OR MONAD SO THAT CARDS ARE PASSED TO
--IT ONE BY ONE AND EVERYTHING GETS UPDATED?

--IF THE "FINAL" FUNCTIONS WHICH CHECK THE CARDS ARE THE SAME IN ANY OF THESE SITUATIONS,
--START WITH THOSE
--      [Card] -> Prob

--foldr'S step VS MONAD BEHAVIOUR

-- STRUCTURING CAN BE THE FOLLOWING:
--  ONE FUNCTION TAKES THE TABLE AND RETURNS Prob OF ALL HandtypeS;
--  THEN THE PLAYER'S HAND IS TAKEN IN AND MAPPED OVER THE PROBABILITIES;
--  EITHER STOP AT THE FIRST 100% OR DO THEM ALL (OR BE LAZY AFTER THE FIRST ONE)

--PROBABLY MAKE THE foldr's step FUNCTION (NEED A BETTER NAME) GLOBAL

-- CONSIDER USING APPLICATIVE FUNCTOR STYLE FOR THIS FUNCTION, WITH CUSTOM FUNCTOR, PERHAPS

--MAKE isStraightFlush return RoyalFlush or StraightFlush; IT OPTIMISES TIME
--MAKE THESE FUNCTIONS BE :: [Card] -> Prob
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
--     where (rF', sF', fl')           = flushesProbs  scs c
--           (fK', fH', tK', tP', oP') = nOfAKindProbs scs c
--           st                        = straightProb  scs c
--           hC'                       = highCardProb  scs c


--aimingFor :: [Prob] -> Prob
-- Perhaps this should just be bestChance

    -- Returns Probabilities of RoyalFlush, StraightFlush and Flush
-- flushesProbs :: [Card] -> Card -> (Prob,Prob,Prob)



    -- Returns Probabilities of FourOfAKind, FullHouse, ThreeOfAKind, TwoPair and OnePair
-- nOfAKindProbs :: [Card] -> Card -> (Prob,Prob,Prob,Prob,Prob)



    -- Returns the Probability of a Straight
-- straightProb :: [Card] -> Card -> Prob
-- straightProb scs c =



    -- Returns the Probability of a HighCard
-- highCardProb :: [Card] -> Card -> Prob
-- highCardProb scs c = Prob HighCard 1 []



