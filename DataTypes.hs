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
--       4 - HandType Checkers
--



---- 0 - IMPORTS ---------------------------------------------------------------

module DataTypes where

import Data.List (sort, sortBy, groupBy)
import Data.Function (on)
import Data.Char (toLower)



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
    where eqField c1 c2 = (suitOrValue c1) == (suitOrValue c2)



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



---- 4 - HANDTYPE CHECKERS -----------------------------------------------------

    -- Return the Value of the highest card
    -- This will always be true; the Maybe is there just for consistency
isHighCard :: [Card] -> Maybe Value
isHighCard = Just . value . last . sort


    -- Return the Value of the N-plet
isPair, isThreeOfAKind, isFourOfAKind :: [Card] -> Maybe Value
isPair         = isNplet 2
isThreeOfAKind = isNplet 3
isFourOfAKind  = isNplet 4


    -- Return the Values of the N-Plets in descending order
isTwoPair, isFullHouse :: [Card] -> Maybe [Value]
isTwoPair   = is2Nplet 2 2
isFullHouse = is2Nplet 3 2


    -- Returns the value of the highest card in the Straight
isStraight :: [Card] -> Maybe Value
isStraight cs = isLenType 5 value $ inOrder cs


    -- Returns the Suit of the Flush
isFlush :: [Card] -> Maybe Suit
isFlush cs = isLenType 5 suit $ suitGroups cs


    -- Returns the Suit and the Value of the highest card in the StraightFlush
    -- Does not simply return a Card (which has the same fields) for pattern matching's sake
    -- NOTE: The first pattern match could have also been the real isStraight,
    --       but inOrder would have been applied twice (not as efficient)
isStraightFlush :: [Card] -> Maybe (Suit,Value)
isStraightFlush cs
    | Just val <- isStraight' , Just sui <- isFlush hocs = Just (sui, val)
    | otherwise = Nothing
        where hocs = head ocs
              isStraight' = isLenType 5 value ocs
              ocs = inOrder cs



    -- Returns the Suit of the RoyalFlush
    -- (If Suit hierarchy were implemented, it could only be Hearts)
isRoyalFlush :: [Card] -> Maybe Suit
isRoyalFlush cs = case isStraightFlush cs of
    Just (s,Ace) -> Just s
    _            -> Nothing


--- General Functions ---

    -- Check whether some cards constitute an N-plet (N=2 => Pair, ...)
isNplet :: Int -> [Card] -> Maybe Value
isNplet n cs = isLenType n value $ valueGroups cs


    -- Check whether some cards constitute two N-plets (N=3 and 2 (or 2 and 3) => FullHouse, ...)
is2Nplet :: Int -> Int -> [Card] -> Maybe [Value]
is2Nplet n m cs
    | length xg >= a && length yg >= b = Just [value x, value y]
    | otherwise                        = Nothing
        where xg@(x:_):yg@(y:_):_ = valueGroups cs
              [b,a] = sort [n,m]


    -- Return a list of lists of cards grouped if of consecutive values
inOrder :: [Card] -> [[Card]]
inOrder cs = sortBy descLength . groupBy' (isPred `on` value) . reverse $ sort cs
    where isPred v1 v2 = case v1 of
            Two -> False
            _   -> v2 == pred v1


    -- Given a list of lists of cards, return the suit or value of the first
    -- list in it if its length is greater than or equal to n
isLenType :: Int -> (Card -> a) -> [[Card]] -> Maybe a
isLenType n suitOrValue cs
    | length hcs >= n = Just . suitOrValue $ head hcs
    | otherwise       = Nothing
        where hcs = head cs



---- 5 - NON-DATATYPE FUNCTIONS ------------------------------------------------

    -- Descending list ordering function. Perfect for: sortBy descLength $ [[a]]
descLength :: [a] -> [a] -> Ordering
descLength l1 l2 = (compare `on` length) l2 l1

    -- Stricter version of groupBy in the sense that does not assume that the
    -- provided comparison function is an equivalence relation.
    -- Only transitivity is assumed here
    -- i.e. This function compares adjacent values, and does not take the
    -- ("wrong") shortcut that the real groupBy does
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _   []  = []
groupBy' _   [x] = [[x]]
groupBy' cmp (x:xs@(x':_))
    | cmp x x'   = (x:y):ys
    | otherwise  = [x]:r
        where r@(y:ys) = groupBy' cmp xs
