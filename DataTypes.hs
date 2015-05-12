---- Poker Analyser Data Types and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.19 - 12/05/2015
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

import Data.List (sort, sortBy, groupBy, splitAt, intersect, union, (\\), nub)
import Data.Char (toLower)
import Data.Function (on)
import qualified Data.Map as M (lookup, fromList)



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
    enumFrom c = dropWhile (<c) . concat $
        fromSVG (enumFrom $ (minBound :: Suit)) (enumFrom $ (minBound :: Value))
-- All the other Enum functions are automatically derived from toEnum and fromEnum


--- Functions ---

    -- Lists of all Cards, Values and Suits
allCards = enumFrom $ Card Two Spades
allValues = enumFrom Two
allSuits = enumFrom Spades


    -- Transform a list of lists of integers in [0..51] into the same for Cards
intsLToCardsL :: [[Int]] -> [[Card]]
intsLToCardsL = map (map toEnum)


    -- Transform a list of Cards into a list of Integers
cardsToInts :: [Card] -> [Int]
cardsToInts = map fromEnum


	-- Generate a list of Cards from lists of Suits and Values grouped by Suits
fromSVG :: [Suit] -> [Value] -> [[Card]]
fromSVG ss vs = [[Card v s | v <- vs] | s <- ss]
    -- Same as: concat . fromSVG
fromSV :: [Suit] -> [Value] -> [Card]
fromSV ss vs = [Card v s | s <- ss,  v <- vs]

	-- Generate a lists of Cards from lists of Values and Suits grouped by Values
fromVSG :: [Value] -> [Suit] -> [[Card]]
fromVSG vs ss = [[Card v s | s <- ss] | v <- vs]
    -- Same as: concat . fromVSG
fromVS :: [Value] -> [Suit] -> [Card]
fromVS vs ss = [Card v s | v <- vs, s <- ss]


    -- Generate a list of Cards by zipping Values and Suits
makeCs :: [Value] -> [Suit] -> [Card]
makeCs vs ss = map (\(v,s)-> Card v s) $ zip vs ss


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


    -- Group cards by value and by suit and sort the groups,
    -- sorted by descending group length and not
valueDescGroups, suitDescGroups, valueGroups, suitGroups :: [Card] -> [[Card]]
valueDescGroups = sortBy descLength . valueGroups
suitDescGroups  = sortBy descLength . suitGroups
valueGroups = groupCardsBy value . sort
suitGroups  = groupCardsBy suit  . sortBySuit


    -- Group cards by descending value or suit assuming they are already sorted
    -- by value or suit, respectively
groupCardsBy :: Eq a => (Card -> a) -> [Card] -> [[Card]]
groupCardsBy suitOrValue = groupBy ((==) `on` suitOrValue) . reverse



---- 2 - HAND RELATED DATA TYPES -----------------------------------------------

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
                deriving (Eq, Ord, Enum, Bounded, Show, Read)


data HandTypesField = HV Value | HS Suit | HL [Value] | HT (Suit,Value)
                deriving (Eq, Ord)
instance Show HandTypesField where
   show (HV x) = show x
   show (HS x) = show x
   show (HL x) = show x
   show (HT x) = show x


data HandTypeCount = HandTypeCount {cType :: HandType, completers :: [[Card]], probs :: [(Int,Float)]}
                deriving (Eq, Show)


data Hand = Hand {hType :: HandType, hTField :: HandTypesField, rank :: Int, cards :: [Card]}
                deriving (Eq, Ord, Show)


data Prob = Prob {pKind :: HandType, chance :: Float, need :: [Either Value Suit]}
                deriving (Eq, Ord, Show)



--- Functions ---

    -- Value Extractors
toV (HV x) = x
toS (HS x) = x
toL (HL x) = x
toT (HT x) = x


    -- Return the list of cards which would, at the present stage, by themselves,
    -- get the player closer the HandType in question
    -- Note: they are ordered by first occurrence by probability of their Hand
anyNeeded :: HandTypeCount -> [Card]
anyNeeded = nub . concat . completers


    -- Return the number of possible HandType Hands in the HandTypeCount
    -- Note: this is more efficient than taking the length of completers
count :: HandTypeCount -> Int
count = sum . map fst . probs


    -- List of "empty" probabilities in descending HandType
noProbs :: [Prob]
noProbs = map (\hT-> Prob hT 0 []) . reverse . enumFrom $ (minBound :: HandType)



---- 3 - STATE RELATED DATA TYPES ----------------------------------------------

data Action = GameStart | SetPlayers Int | SetUser Int | SetBalance Int | SetDealer Int | Discard Int
                | RoundStart | StartHand [Card] | Flop [Card] | Turn [Card] | River [Card]
                | Idle | Check Int | Bet Int Int | Raise Int Int | Fold Int | Out Int | Won [Int] Int
                | RoundEnd | GameEnd Int
                deriving (Eq, Ord, Show)


data Player = Player {num :: Int, balance :: Int, onPlate :: Int, status :: Action,
                        hisCards :: [Card], hisHand :: Hand}
                deriving (Eq, Ord)
    -- EVENTUALLY INTRODUCE STATISTICS TRACKING IN HERE
instance Show Player where
    show pl = concat [  "\n",
                        "\tPlayer: ",  show $ num pl,
                        ", Balance: ", show $ balance pl,
                        ", Bet: ",     show $ onPlate pl,
                        ", Status: ",  show $ status pl,
                        "\n\t",
                        "\tCards: ",   show $ hisCards pl,
                        "\n\t",
                        "\tHand: ",    show $ hisHand pl]


data PlayerField = PI Int | PA Action | PC [Card] | PH Hand
                deriving (Eq, Show)


    -- NOTE: The Deck also includes all the cards in other players' hands and discarded ones
data Deck = Deck {cardsIn :: Int, valuesIn :: [Int], suitsIn :: [Int]}
                deriving (Eq, Show)


data Frame = Frame {action :: Action, playersNum :: Int, userNum :: Int,
                    dealer :: Int, deck :: Deck, table :: [Card],
                    plate :: Int, players :: [Player]}
                deriving (Eq)
                    -- The actual player is the first (0) in players list
instance Show Frame where
    show fr = unlines $ map (\sf-> (fst sf) ++ ((snd sf) fr)) funcs
        where funcs :: [(String,(Frame -> String))]
              funcs = [ ("Current action: ",    show . action),
                        ("Players Number: ",    show . playersNum),
                        ("User Number: ",       show . userNum),
                        ("Dealer ID: ",         show . dealer),
                        ("Deck: ",              show . deck),
                        ("Cards on table: ",    show . table),
                        ("Amount on plate: ",   show . plate),
                        ("Players' Status: ",   show . players) ]


data FrameField = FA Action | FI Int | FD Deck | FC [Card] | FP [Player]
                deriving (Eq, Show)


type State = [Frame]



--- Functions ---

    -- Player Value Extractors
fromPI (PI x) = x
fromPA (PA x) = x
fromPC (PC x) = x
fromPH (PH x) = x


    -- Field Value Extractors
fromFA (FA x) = x
fromFI (FI x) = x
fromFD (FD x) = x
fromFC (FC x) = x
fromFP (FP x) = x


    -- Starting Player
initialPlayer :: Int -> Player
initialPlayer nu = Player nu 0 0 Idle [] (Hand HighCard (HV Two) 0 [])


    -- Starting Deck
initialDeck :: Deck
initialDeck = Deck 52 (replicate 13 4) (replicate 4 13)


    -- The User's Cards
myCards :: Frame -> [Card]
myCards f = hisCards . head . filter ((== userNum f) . num) $ players f


    -- Starting State
initialState :: State
initialState = [Frame GameStart 0 0 0 initialDeck [] 0 []]


    -- Generate a Player by providing only the fields which change
    -- with respect to the previous one
newPlayer :: Player -> [(String, PlayerField)] -> Player
newPlayer pl fieldList = (Player no ba op st cs hh)
    where no = newField num      fromPI "num"
          ba = newField balance  fromPI "balance"
          op = newField onPlate  fromPI "onPlate"
          st = newField status   fromPA "status"
          cs = newField hisCards fromPC "hisCards"
          hh = newField hisHand  fromPH "hisHand"

          newField :: (Player -> a) -> (PlayerField -> a) -> String -> a
          newField field extractor key =
                maybe (field pl) extractor . M.lookup key $ M.fromList fieldList


    -- Add a new Frame to the State by providing only the fields which change
    -- with respect to the previous one
addFrame :: State -> [(String, FrameField)] -> State
addFrame s@(f:_) fieldList = (Frame act plN usN dea dCN tab plt pls):s
    where act = newField action      fromFA "action"
          plN = newField playersNum  fromFI "playersNum"
          usN = newField userNum     fromFI "userNum"
          dea = newField dealer      fromFI "dealer"
          dCN = newField deck        fromFD "deck"
          tab = newField table       fromFC "table"
          plt = newField plate       fromFI "plate"
          pls = newField players     fromFP "players"

          newField :: (Frame -> a) -> (FrameField -> a) -> String -> a
          newField field extractor key =
                maybe (field f) extractor . M.lookup key $ M.fromList fieldList


    -- Generate a new Deck by providing the cards which have been taken out from
    -- the previous one
newDeck :: Deck -> [Card] -> Deck
newDeck d cs = foldr takeOut d cs
    where takeOut (Card v s) (Deck ci vsi ssi) = Deck nCi nVsi nSsi
            where nCi = ci - 1
                  nVsi = (init bvs) ++ ((last bvs) - 1):avs
                  (bvs,avs) = splitAt (1 + fromEnum v) vsi
                  nSsi = (init bss) ++ ((last bss) - 1):ass
                  (bss,ass) = splitAt (1 + fromEnum s) ssi














