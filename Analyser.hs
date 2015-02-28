---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.4 - 27-28/02/2015
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
--       1 - Imports and Type declarations
--       2 - Main Functions
--       3 - Other Functions
--

---- 0 - TO DO and TO CONSIDER -------------------------------------------------

--MAKE Probability A FUNCTOR? MONAD? WHICH IS PASSED AROUND,
--AND WHEN 100% RETURN A Hand, FALL THROUGH WITH IT (THIS IN THE bestHand FUNCTION)

--ADD THE WHOLE CARD LIST TO THE Probability DATA TYPE TO CARRY AROUND?

--foldr'S step VS MONAD BEHAVIOUR

-- STRUCTURING CAN BE THE FOLLOWING:
--  ONE FUNCTION TAKES THE TABLE AND RETURNS Probability OF ALL HandtypeS;
--  THEN THE PLAYER'S HAND IS TAKEN IN AND MAPPED OVER THE PROBABILITIES;
--  EITHER STOP AT THE FIRST 100% OR DO THEM ALL (OR BE LAZY AFTER THE FIRST ONE)

--PROBABLY MAKE THE foldr's step FUNCTION (NEED A BETTER NAME) GLOBAL

-- CONSIDER USING APPLICATIVE FUNCTOR STYLE FOR THIS FUNCTION, WITH CUSTOM FUNCTOR, PERHAPS

--MAKE isStraightFlush return RoyalFlush or StraightFlush; IT OPTIMISES TIME
--MAKE THESE FUNCTIONS BE :: [Card] -> Probability
-- MAKE ALL THESE FUNCTIONS ASSUME THE PREVIOUS ONE HAS RUN?
-- MAKE THEM WORK BY COUNTING THE CARDS THAT ARE NOT "OUT"?
-- AND PERHAPS ALL POSSIBLE OTHER PLAYERS' HANDS?



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

data Probability = Probability {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)



---- 2 - MAIN FUNCTIONS --------------------------------------------------------

main = do
    line <- getLine
    let myHand = read line :: (Suit, Value)
    print $ Card (fst myHand) (snd myHand)



---- 3 - OTHER FUNCTIONS -------------------------------------------------------

-- bestHand :: [Card] -> Hand
-- bestHand cs = getBestHand $ sort cs

--isStraightFlush :: [Card] -> Probability
--isStraightFlush cs = foldr step (pure :: Probability) cs ???????

--
--

