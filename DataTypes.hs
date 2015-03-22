---- Poker Analyser Data Types and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.1 - 20-21/03/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the used data types and all their related
--          functions.
--
--   Sections:
--       0 - Imports
--       1 - Card Related Data Types
--       2 - Hand Related Data Types
--       3 - State Related Data Types
--



---- 0 - IMPORTS ---------------------------------------------------------------

module DataTypes where

import Data.List (sortBy)
import Data.Char (toLower)



---- 1 - CARD RELATED DATA TYPES -----------------------------------------------

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Suit = Spades | Clubs | Diamonds | Hearts
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



--- Functions ---

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



---- 2 - HAND RELATED DATA TYPES -----------------------------------------------

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Hand = Hand {hKind :: HandType, cards :: [Card]}
                deriving (Eq, Ord, Show)

data Prob = Prob {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)



--- Functions ---

    -- List of "empty" probabilities in descending HandType
noProbs :: [Prob]
noProbs = map (\hT-> Prob hT 0 []) . reverse . enumFrom $ (minBound :: HandType)



---- 3 - STATE RELATED DATA TYPES ----------------------------------------------

data Action = GameStart | SetPlayers | SetDealer | Discard | RoundEnd
                | StartHand | Flop | Turn | River | GameEnd
                | Check | Bet | Raise | Fold | Out
                deriving (Eq, Ord, Show)
    -- PERHAPS SPLIT INTO Stage AND Action, WHERE ACTION HAS PLAYER AND AMOUNT FIELDS

data Player = Player {num :: Int, balance :: Int, onPlate :: Int, status :: Action}
                deriving (Eq, Ord, Show)
    -- EVENTUALLY INTRODUCE STATISTICS TRACKING IN HERE

data Frame = Frame {action :: Action, playersNum :: Int, dealer :: Int,
                    cardsInDeck :: Int, table :: [Card], myCards :: [Card],
                    plate :: Int, players :: [Player]}
                deriving (Eq, Show)
                    -- The actual player is the first (0) in players list

data FrameField = FA Action | FI Int | FC [Card] | FP [Player]
                deriving (Eq, Show)

type State = [Frame]



--- Functions ---

toA (FA x) = x
toI (FI x) = x
toC (FC x) = x
toP (FP x) = x

