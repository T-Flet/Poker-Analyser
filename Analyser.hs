---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.3
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

-- CONSIDER USING APPLICATIVE FUNCTOR STYLE FOR THIS FUNCTION, WITH CUSTOM FUNCTOR, PERHAPS
-- OR MONADIC STYLE, PERHAPS do NOTATION
-- AND THE maybe FUNCTION TO EXTRACT HANDS FROM Maybe TYPES
--          AMENDMENT: FIND Either EQUIVALENT OF maybe FUNCTION
-- USE succ FROM Enum TYPECLASS FOR NEXT ELEMENT IN CLASS
--          FIND A WAY TO MAKE succ WORK WITH Card
--          OR, ALTERNATIVELY, MAKE A FUNCTION WICH CAN LIST CARDS, ALL OR NOT

--PERHAPS MAKE isStraightFlush return RoyalFlush or StraightFlush; it optimises time
--PERHAPS MAKE THESE FUNCTIONS BE :: [Card] -> Either Probability Hand

-- MAKE ALL THESE FUNCTIONS ASSUME THE PREVIOUS ONE HAS RUN?
-- MAKE THEM WORK BY COUNTING THE CARDS THAT ARE NOT "OUT"?
-- AND PERHAPS ALL POSSIBLE OTHER PLAYERS' HANDS?

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sort)


data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
                deriving (Eq, Ord, Enum, Show, Read)
data Suit = Spades | Clubs | Diamonds | Hearts
                deriving (Eq, Ord, Enum, Show, Read)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Enum, Show, Read)

data Card = Card {value :: Value, suit :: Suit}
                deriving (Eq, Ord, Show)
data Hand = Hand {hKind :: HandType, cards :: [Card]}
                deriving (Eq, Ord, Show)
data Probability = Probability {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)


main = do
    line <- getLine
    let myHand = read line :: (Value, Suit)
    print $ Card (fst myHand) (snd myHand)



-- bestHand :: [Card] -> Hand
-- bestHand cs
--     | isRoyalFlush    scs = RoyalFlush
--     | isStraightFlush scs = StraightFlush
--     | isFourOfAKind   scs = FourOfAKind
--     | isFullHouse     scs = FullHouse
--     | isFlush         scs = Flush
--     | isStraight      scs = Straight
--     | isThreeOfAKind  scs = ThreeOfAKind
--     | isTwoPair       scs = TwoPair
--     | isOnePair       scs = OnePair
--     | isHighCard      scs = HighCard
--         where scs = sort cs

--isRoyalFlush :: [Card] -> Either Probability Hand

