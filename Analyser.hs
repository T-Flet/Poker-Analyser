---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.0
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

-- type Hand = (Card v1 s1, Card v2 s2) deriving (Eq, Ord, Show, Read)


main = do
    print "Hello, world!"
