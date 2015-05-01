---- Poker Analyser Data Types and Related Functions
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.14 - 01/05/2015
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

import Data.List (sort, sortBy, groupBy, splitAt, intersect, union, (\\))
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
        fromSuiVal (enumFrom $ (minBound :: Suit)) (enumFrom $ (minBound :: Value))
-- All the other Enum functions are automatically derived from toEnum and fromEnum


data CardSet = CC Int [Card] | CB Int (Value,Value) | CV Int Value | CS Int Suit
            | CSV Int [Suit] [Value] | CVS Int [Value] [Suit]
            | CA CardSet CardSet | CO CardSet CardSet | CD CardSet CardSet | CN
                deriving (Eq)
instance Show CardSet where
    show (CC 1 x) = "Any 1 of " ++ show x
    show (CC n x) = show n ++ " of " ++ show x
    show (CV 1 x) = "Any 1 " ++ show x
    show (CV n x) = "Any " ++ show n ++ " " ++ show x ++ "s"
    show (CS 1 x) = "Any 1 " ++ (init $ show x)
    show (CS n x) = "Any " ++ show n ++ " " ++ show x

    show (CSV n ss vs) = "Any " ++ show n ++ " of " ++ show ss ++ " of " ++ show vs
    show (CVS n vs ss) = "Any " ++ show n ++ " of " ++ show vs ++ " of " ++ show ss

    show (CB 1 (f,l)) = "Any 1 between " ++ show f ++ " and " ++ show l
    show (CB n (f,l)) = show n ++ " between " ++ show f ++ " and " ++ show l

    show (CA c1 c2) = "Both "   ++ show c1 ++ " and " ++ show c2
    show (CO c1 c2) = "Either " ++ show c1 ++ " or "  ++ show c2

    show (CD c1 c2) = show c1 ++ " without " ++ show c2

    show CN = "No Cards"



--- Functions ---

    -- List of all Cards
allCards = enumFrom $ Card Two Spades


    -- Transform a list of lists of integers in [0..51] into the same for Cards
intsLToCardsL :: [[Int]] -> [[Card]]
intsLToCardsL = map (map toEnum)


    -- Transform a list of Cards into a list of Integers
cardsToInts :: [Card] -> [Int]
cardsToInts = map fromEnum


	-- Generate a list of lists of Cards from lists of Suits and Values in this order
fromSuiVal :: [Suit] -> [Value] -> [[Card]]
fromSuiVal ss vs = [[Card v s | v <- vs] | s <- ss]


	-- Generate a list of lists of Cards from lists of Values and Suits in this order
fromValSui :: [Value] -> [Suit] -> [[Card]]
fromValSui vs ss = [[Card v s | s <- ss] | v <- vs]


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


    -- Transform a CardSet into its equivalent list of Cards
fromCardSet :: CardSet -> [Card]
fromCardSet (CC n cs)     = cs
fromCardSet (CB n (f,l))  = concat $ fromValSui (enumFromTo f l) (enumFrom Spades)
fromCardSet (CV n v)      = head . fromValSui [v] $ enumFrom Spades
fromCardSet (CS n s)      = enumFromTo (Card Two s) (Card Ace s)
fromCardSet (CSV n ss vs) = concat $ fromSuiVal ss vs
fromCardSet (CVS n vs ss) = concat $ fromValSui vs ss
fromCardSet (CA a b)      = (intersect `on` fromCardSet) a b
fromCardSet (CO a b)      = (union `on` fromCardSet) a b
fromCardSet (CD a b)      = ((\\) `on` fromCardSet) a b
fromCardSet CN            = []


    -- Return the length of a CardSet more efficiently than counting its cards
lengthCardSet :: CardSet -> Int
lengthCardSet (CC n cs)     = length cs
lengthCardSet (CB n (f,l))  = 4 * (fromEnum l - fromEnum f)
lengthCardSet (CV n v)      = 4
lengthCardSet (CS n s)      = 13
lengthCardSet (CSV n ss vs) = length ss * length vs
lengthCardSet (CVS n vs ss) = length vs * length ss
lengthCardSet (CA a b)      = abs $ ((-) `on` lengthCardSet) a b
lengthCardSet (CO a b)      = ((+) `on` lengthCardSet) a b
lengthCardSet cd@(CD a b)   = length $ fromCardSet cd
lengthCardSet CN            = 0



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


data HandTypeCount = HandTypeCount {cType :: HandType, count :: Int, needed :: CardSet, probs :: [(Int,Float)]}
                deriving (Eq, Show)
                    -- "needed" is all the cards which, if drawn by themselves
                    -- (only one of them), will get the already out cards closer
                    -- to the intended HandType or a better instance of it


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














