---- Poker Analyser Data Types and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.6 - 27-28/03/2015
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

import GeneralFunctions (descLength)

import Data.List (sort, sortBy, groupBy)
import Data.Char (toLower)
import Data.Function (on)



---- 1 - CARD RELATED DATA TYPES -----------------------------------------------

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Suit = Spades | Clubs | Diamonds | Hearts
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card {value :: Value, suit :: Suit}
                deriving (Eq, Ord)
instance Show Card where
    show c = (show $ value c) ++ " of " ++ (show $ suit c)
instance Enum Card where
    toEnum i   = Card (toEnum (i`mod`13) :: Value) (toEnum (i`div`13) :: Suit)
    fromEnum c = (fromEnum $ value c) + ((*13) . fromEnum $ suit c)
        -- This enumFrom is in deck order (suit first), while actual card
        -- comparison is by value first
    enumFrom c = dropWhile (<c) [Card v s |
        s <- enumFrom $ (minBound :: Suit), v <- enumFrom $ (minBound :: Value)]
-- All the other Enum functions are automatically derived from toEnum and fromEnum

    -- List of all Cards
allCards = enumFrom $ Card Two Spades

    -- Transform a list of lists of integers in [0..51] into the same for Cards
intsLToCardsL :: [[Int]] -> [[Card]]
intsLToCardsL = map (map toEnum)

    -- Transform a list of Cards into a list of Integers
cardsToInts :: [Card] -> [Int]
cardsToInts = map fromEnum



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
    -- NOTE: Did not make this a Read instance because I need the Maybe
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


    -- Group cards by value and by suit
valueGroups, suitGroups :: [Card] -> [[Card]]
valueGroups = groupCardsBy value . sort
suitGroups  = groupCardsBy suit  . sortBySuit

    -- Group cards by value or suit assuming they are already sorted
    -- by value or suit, respectively
groupCardsBy :: Eq a => (Card -> a) -> [Card] -> [[Card]]
groupCardsBy suitOrValue = sortBy descLength . groupBy eqField
    where eqField = (==) `on` suitOrValue



---- 2 - HAND RELATED DATA TYPES -----------------------------------------------

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Hand = Hand {hKind :: HandType, cards :: [Card]}
                deriving (Eq, Ord, Show)

data Prob = Prob {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)


data HandTypesField = HV Value | HS Suit | HL [Value] | HT (Suit,Value)
                deriving (Eq)
instance Show HandTypesField where
   show (HV x) = show x
   show (HS x) = show x
   show (HL x) = show x
   show (HT x) = show x



--- Functions ---

    -- Value Extractors
toV (HV x) = x
toS (HS x) = x
toL (HL x) = x
toT (HT x) = x


    -- List of "empty" probabilities in descending HandType
noProbs :: [Prob]
noProbs = map (\hT-> Prob hT 0 []) . reverse . enumFrom $ (minBound :: HandType)



---- 3 - STATE RELATED DATA TYPES ----------------------------------------------

data Action = GameStart | SetPlayers Int | SetDealer Int | Discard Int
                | RoundStart | StartHand [Card] | Flop [Card] | Turn [Card] | River [Card]
                | Idle | Check Int | Bet Int Int | Raise Int Int | Fold Int | Out Int
                | RoundEnd | GameEnd Int
                deriving (Eq, Ord, Show)

data Player = Player {num :: Int, balance :: Int, onPlate :: Int, status :: Action}
                deriving (Eq, Ord)
    -- EVENTUALLY INTRODUCE STATISTICS TRACKING IN HERE
instance Show Player where
    show pl = concat [  "\n",
                        "\tPlayer: ",   show $ num pl,
                        ", Balance: ",  show $ balance pl,
                        ", Bet: ",      show $ onPlate pl,
                        ", Status: ",   show $ status pl]

data Frame = Frame {action :: Action, playersNum :: Int, dealer :: Int,
                    cardsInDeck :: Int, table :: [Card], myCards :: [Card],
                    plate :: Int, players :: [Player]}
                deriving (Eq)
                    -- The actual player is the first (0) in players list
instance Show Frame where
    show fr = unlines $ map (\sf-> (fst sf) ++ ((snd sf) fr)) funcs
        where funcs :: [(String,(Frame -> String))]
              funcs = [ ("Current action: ",    show . action),
                        ("Players Number: ",    show . playersNum),
                        ("Dealer ID: ",         show . dealer),
                        ("Cards in deck: ",     show . cardsInDeck),
                        ("Cards on table: ",    show . table),
                        ("My Cards: ",          show . myCards),
                        ("Amount on plate: ",   show . plate),
                        ("Players' Status: ",   show . players) ]


data FrameField = FA Action | FI Int | FC [Card] | FP [Player]
                deriving (Eq, Show)

type State = [Frame]



--- Functions ---

    -- Value Extractors
toA (FA x) = x
toI (FI x) = x
toC (FC x) = x
toP (FP x) = x


    -- Starting state
initialState :: State
initialState = [Frame GameStart 0 0 52 [] [] 0 []]



