---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.2
--
--      Description:
--          Poker analysing shell.
--          Inupts:
--              Player's cards, table cards (later on: player's fiches and even
--              later other players' as well).
--              Possible user request for (his or other's) probability of a
--              specific hand.
--          Outputs:
--              Probabilty of player's or others' specific hand.
--              Specific request response.
--              (Later: suggested bet)
--
--   Sections:
--       1 - Imports and Type declarations
--

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

data Card = Card {value :: Value, suit :: Suit} deriving (Eq, Ord, Show, Read)
data Suit = Spades | Clubs | Diamonds | Hearts deriving (Eq, Ord, Show, Read)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace deriving (Eq, Ord, Show, Read)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Show, Read)
data Hand = Hand {kind :: HandType, cards :: [Card]}
                deriving (Eq, Ord, Show, Read)


main = do
    print "Hello, world!"


-- CONSIDER USING APPLICATIVE FUNCTOR STYLE FOR THIS FUNCTION, WITH CUSTOM FUNCTOR, PERHAPS
-- AND THE maybe FUNCTION TO EXTRACT HANDS FROM Maybe TYPES
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

--isRoyalFlush :: [Card] -> Maybe Hand
