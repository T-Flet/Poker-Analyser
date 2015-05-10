---- Poker Analyser Unused Stuff
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.1 - 10-11/05/2015
--
--      Description:
--          Poker analysing shell.
--          This package contains all the stuff which was developed and worked
--          but was then removed for some reason, but which is also worthy
--          enough to keep around.
--
--   Sections:
--       0 - Imports
--       1 - CardSet
--



---- 0 - IMPORTS ---------------------------------------------------------------

import DataTypes

import Data.List (union, intersect, (\\))
import Data.Function (on)



---- 1 - CARDSET ---------------------------------------------------------------

data CardSet = CN | CC [Card] | CCC [[Card]] | CV [Value] | CS [Suit]
            | CB (Value,Value) | CSV [Suit] [Value] | CVS [Value] [Suit]
            | CA CardSet CardSet | CO CardSet CardSet | CD CardSet CardSet
                deriving (Eq)
instance Show CardSet where
    show CN          = "No Cards"
    show (CCC x)     = "Any of " ++ show x
    show (CC x)      = "Any of " ++ show x
    show (CV x)      = "Any " ++ show x
    show (CS x)      = "Any " ++ show x
    show (CB (f,l))  = "Any between " ++ show f ++ " and " ++ show l
    show (CSV ss vs) = "Any " ++ " of " ++ show ss ++ " of " ++ show vs
    show (CVS vs ss) = "Any " ++ " of " ++ show vs ++ " of " ++ show ss
    show (CA c1 c2)  = "Both "   ++ show c1 ++ " and " ++ show c2
    show (CO c1 c2)  = "Either " ++ show c1 ++ " or "  ++ show c2
    show (CD c1 c2)  = show c1 ++ " without " ++ show c2


    -- Matching HandTypeCount Type
--data HandTypeCount = HandTypeCount {cType :: HandType, count :: Int, needed :: CardSet, probs :: [(Int,Float)]}
--                deriving (Eq, Show)


-- Functions --

    -- Transform a CardSet into its equivalent list of Cards
cardSetCs :: CardSet -> [Card]
cardSetCs CN          = []
cardSetCs (CCC css)   = foldr union [] css
cardSetCs (CC cs)     = cs
cardSetCs (CV vs)     = head . fromVSG vs $ enumFrom Spades
cardSetCs (CS ss)     = concat $ map (\s-> enumFromTo (Card Two s) (Card Ace s)) ss
cardSetCs (CB (f,l))  = concat $ fromVSG (enumFromTo f l) (enumFrom Spades)
cardSetCs (CSV ss vs) = concat $ fromSVG ss vs
cardSetCs (CVS vs ss) = concat $ fromVSG vs ss
cardSetCs (CA a b)    = (intersect `on` cardSetCs) a b
cardSetCs (CO a b)    = (union     `on` cardSetCs) a b
cardSetCs (CD a b)    = ((\\)      `on` cardSetCs) a b


    -- Return the length of a CardSet more efficiently than counting its cards
cardSetLen :: CardSet -> Int
cardSetLen CN          = 0
cardSetLen (CCC css)   = length $ foldr union [] css
cardSetLen (CC cs)     = length cs
cardSetLen (CV vs)     = 4 * length vs
cardSetLen (CS ss)     = 13 * length ss
cardSetLen (CB (f,l))  = 4 * (fromEnum l - fromEnum f)
cardSetLen (CSV ss vs) = length ss * length vs
cardSetLen (CVS vs ss) = length vs * length ss
cardSetLen (CA a b)    = abs $ ((-) `on` cardSetLen) a b
cardSetLen (CO a b)    = ((+) `on` cardSetLen) a b
cardSetLen cd@(CD a b) = length $ cardSetCs cd


    -- Given a CA, CO or CD, return the most appropriate other CardSet
    -- Other constructors return themselves
cardSetApp :: CardSet -> CardSet
cardSetApp (CA x y) = handleCons 'A' x y
cardSetApp (CO x y) = handleCons 'O' x y
cardSetApp (CD x y) = handleCons 'D' x y
cardSetApp otherCon = otherCon

    -- Handle each pair of CardSets with the appropriate list function
handleCons :: Char -> CardSet -> CardSet -> CardSet
handleCons op c1 c2 = let
        listOp :: Eq a => Char -> [a] -> [a] -> [a]
        listOp 'A' = intersect
        listOp 'O' = union
        listOp 'D' = (\\)
    in case (c1, c2) of
    (CN,        b        ) -> case op of 'A' -> CN ; 'O' -> b ; 'D' -> CN
    (a,         CN       ) -> case op of 'A' -> CN ; 'O' -> a ; 'D' -> a
    (CCC a,     CCC b    ) -> CCC $ listOp op a b
    (CC  a,     CC  b    ) -> CC  $ listOp op a b
    (CV  a,     CV  b    ) -> CV  $ listOp op a b
    (CS  a,     CS  b    ) -> CS  $ listOp op a b
    (CV  a,     CS  b    ) -> case op of
                                'A' -> CVS a b
                                _   -> CC $ (listOp op `on` cardSetCs) (CV a) (CS b)
    (CS  a,     CV  b    ) -> case op of
                                'A' -> CSV a b
                                _   -> CC $ (listOp op `on` cardSetCs) (CS a) (CV b)
    (CB  (f,l), CB  (g,m)) -> case op of
                                'A' -> CB (max f g, min l m)
                                'O' -> CB (min f g, max l m)
                                'D' -> CV $ (\\) (enumFromTo f l) (enumFromTo g m)
    (CV  a,     CB  (f,l)) -> CV  $ listOp op a (enumFromTo f l)
    (CB  (f,l), CV  b    ) -> CV  $ listOp op (enumFromTo f l) b
    (CS  a,     CB  (f,l)) -> CSV a (enumFromTo f l)
    (CB  (f,l), CS  b    ) -> CVS (enumFromTo f l) b
    (CSV as av, CSV bs bv) -> CSV (listOp op as bs) (listOp op av bv)
    (CVS av as, CVS bv bs) -> CVS (listOp op av bv) (listOp op as bs)
    (CSV as av, CVS bv bs) -> CSV (listOp op as bs) (listOp op av bv)
    (CVS av as, CSV bs bv) -> CVS (listOp op av bv) (listOp op as bs)
    (CSV as av, CV  bv   ) -> CSV as (listOp op av bv)
    (CV  av,    CSV bs bv) -> CSV bs (listOp op av bv)
    (CSV as av, CS  bs   ) -> CSV (listOp op as bs) av
    (CS  as,    CSV bs bv) -> CSV (listOp op as bs) bv
    (CSV as av, CB  (f,l)) -> CSV as (listOp op av (enumFromTo f l))
    (CB  (f,l), CSV bs bv) -> CSV bs (listOp op (enumFromTo f l) bv)
    (CVS av as, CV  bv   ) -> CVS (listOp op av bv) as
    (CV  av,    CVS bv bs) -> CVS (listOp op av bv) bs
    (CVS av as, CS  bs   ) -> CVS av (listOp op as bs)
    (CS  as,    CVS bv bs) -> CVS bv (listOp op as bs)
    (CVS av as, CB (f,l) ) -> CVS (listOp op av (enumFromTo f l)) as
    (CB  (f,l), CVS bv bs) -> CVS (listOp op (enumFromTo f l) bv) bs
    (CA a1 a2,  b        ) -> handleCons op (handleCons 'A' a1 a2) b
    (a,         CA b1 b2 ) -> handleCons op a (handleCons 'A' b1 b2)
    (CO a1 a2,  b        ) -> handleCons op (handleCons 'O' a1 a2) b
    (a,         CO b1 b2 ) -> handleCons op a (handleCons 'O' b1 b2)
    (CD a1 a2,  b        ) -> handleCons op (handleCons 'D' a1 a2) b
    (a,         CD b1 b2 ) -> handleCons op a (handleCons 'D' b1 b2)
    (a,         b        ) -> CC  $ (listOp op `on` cardSetCs) a b



