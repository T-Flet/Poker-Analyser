---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.17 - 21-22/03/2015
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
--       0 - Imports
--       1 - Main Functions
--       2 - Shell Direct Functions
--       3 - Shell Data Manipulation Functions
--



---- 0 - IMPORTS ---------------------------------------------------------------

import DataTypes
import Probabilities

import Data.List (sort, sortBy, groupBy, maximumBy)
import qualified Data.Map as M (lookup, fromList)



---- 1 - MAIN FUNCTIONS --------------------------------------------------------

main = do
    putStrLn "Analyser Shell"
    gameShell initialState



    -- PERHAPS THIS CAN BE DONE WITH foldM WITH THE STATE AS THE ACCUMULATOR
gameShell :: State -> IO ()
gameShell state = do
    cmd <- getLine
    if cmd == "q"
        then do
            putStrLn "Game Over"
            putStrLn $ "The players' balances are " ++ (show $ balances state)
            putStrLn $ "Player " ++ (show $ inTheLead state) ++ " wins"
        else do
            let (newState, msg) = shellCommand state cmd
            putStrLn msg
            gameShell newState


    -- PERHAPS LATER MAKE WITH REGEXES (IF MAKING IT SAFER IS DIFFICULT IN THIS WAY)
    -- TIDY UP:
    --  COULD INCLUDE THE TUPLE IN THE SINGLE FUNCTIONS INSTEAD OF HERE
    --  COULD USE A LET OR WHERE CLAUSE TO MAKE THE CHARS STRINGS INSTEAD OF DOING, FOR EXAMPLE, [x]
shellCommand :: State -> String -> (State,String)
shellCommand s cmd = case cmd of
    -- Player related commands start with p
        -- Set players number
    ('p':'n':' ':n) ->
                (setPlayers s (read n :: Int),
                    "Players number set to " ++ n)
        -- Player x is dealer (the actual player is the first in whichever direction)
    ('p':x:'d':_) ->
                (setDealer s (read [x] :: Int),
                    "Player " ++ [x] ++ " is dealer")
        -- Player x Folds
    ('p':x:'f':_) ->
                (plFolds s (read [x] :: Int),
                    "Player " ++ [x] ++ " folded")
        -- Player x Bets (or Raises, but reporting the bet) by amount
    ('p':x:'b':' ':a) ->
                (plBets s Bet (read [x] :: Int) (read a :: Int),
                    "Player " ++ [x] ++ " bet " ++ a)
        -- Player x Raises by amount
    ('p':x:'r':' ':a) ->
                (plBets s Raise (read [x] :: Int) (read a :: Int),
                    "Player " ++ [x] ++ " raised " ++ a)

        -- Back one action
    "b" ->
                (tail s,
                    "Revoked last action: " ++ (show . action $ head s))

    -- Card related commands start with p
        -- Discard n cards (it can happen)
    ('c':'d':' ':n) ->
                (discard s (read n :: Int),
                    "Discarded " ++ n ++ " cards")
        -- Set initial hand
    ('c':'i':' ':v1:s1:' ':v2:s2:_) ->
                cardHandler s StartHand [[v1,s1],[v2,s2]]
        -- Flop
    ('c':'f':' ':v1:s1:' ':v2:s2:' ':v3:s3:_) ->
                cardHandler s Flop  [[v1,s1],[v2,s2],[v3,s3]]
        -- Turn
    ('c':'t':' ':v1:s1:_) ->
                cardHandler s Turn  [[v1,s1]]
        -- River
    ('c':'r':' ':v1:s1:_) ->
                cardHandler s River [[v1,s1]]

    -- Frame related commands begin with f
        -- Show any field from current frame
    ('f':'f':' ':field) ->
                fieldHandler s field

        -- Show the full frame fields
    "fff" ->
                (s, show $ head s)

        -- Show players' balances
    "fb" ->
                (s, show $ balances s)

        -- Number of actions or frames
    "fa" ->
                (s, "Frames: " ++ (show $ length s))

        -- MAKE A COMMAND TO DISPLAY THE NUMBER OF ROUNDS (CALCULATE OR STORE IT)
--    ('h':_) ->
--                (s, help)

-- COMMAND TO PRINT THE CURRENT FRAME IN HUMAN READABLE FORMAT OR SOMETHING
-- AND SPECIFIC ONES (OR A GENERAL ONE WHICH TAKES A FIELD AS ARGUMENT)
--  WHICH RETURNS SOME VALUE FROM THE STATE (MOSTLY LAST FRAME)

        -- Otherwise: not a recognised command
    _            -> (s, "Command not recognised")



---- 2 - SHELL DIRECT FUNCTIONS ------------------------------------------------

    -- Show players' balances
balances :: State -> [(Int,Int)]
balances (f:_) = foldr extractBal [] $ players f
    where extractBal pl bs = (num pl, balance pl):bs


    -- Determine the player currently in the lead
inTheLead :: State -> Int
inTheLead = fst . maximumBy cmpBal . balances
    where cmpBal x y = compare (snd x) (snd y)


    -- Starting state
initialState :: State
initialState = [Frame GameStart 0 0 52 [] [] 0 []]


    -- Set the number of players
setPlayers :: State -> Int -> State
setPlayers s n = newFrame s [("action", FA SetPlayers), ("playersNum", FI n)]


    -- Set the player x (x after the actual player) to be the dealer
setDealer :: State -> Int -> State
setDealer s p = newFrame s [("action", FA SetDealer), ("dealer", FI p)]


    -- Give out two cards per player
startHand :: State -> [Card] -> State
startHand s@(f:_) cs = newFrame s [("action", FA StartHand), ("myCards", FC cs), ("cardsInDeck", FI ndcs)]
    where ndcs = (cardsInDeck f) - 2 * (playersNum f)


    -- Discard n cards (for some reason)
discard :: State -> Int -> State
discard s@(f:_) n = newFrame s [("action", FA Discard), ("cardsInDeck", FI ndcs)]
    where ndcs = (cardsInDeck f) - n


    -- Add some cards to the table (Flop, Turn, River)
addCards :: State -> Action -> [Card] -> State
addCards s@(f:_) act cs = newFrame s [("action", FA act), ("cardsInDeck", FI ndcs), ("table", FC ntab)]
    where ndcs = (cardsInDeck f) - (length cs) - discdCs
          ntab = cs ++ (table f)
          discdCs
            | act == Flop = 3
            | otherwise   = 1


    -- Player x Folds
plFolds :: State -> Int -> State
plFolds s@(f:_) x = newFrame s [("action", FA Fold), ("players", FP npls)]
    where npls = map plStatus (players f)
          plStatus pl
            | num pl == x = Player x (balance pl) (onPlate pl) Fold
            | otherwise   = pl


    -- INTRODUCE NEGATIVE BALANCE CHECKS SOMEWHERE
    -- ALSO, CHECK THAT A Bet IS >= THE PREVIOUS ONE
    -- Player x bets or raises by amount a
plBets :: State -> Action -> Int -> Int -> State
plBets s@(f:_) act x a = newFrame s [("action", FA act), ("players", FP npls)]
    where npls = map plStatus (players f)
          plStatus pl
            | num pl == x && act == Bet   = Player x nBal nPlt act
            | num pl == x && act == Raise = Player x (nBal-pPPlt) (nPlt+pPPlt) act
            | otherwise                   = pl
                where nBal = ((balance pl) - a)
                      nPlt = ((onPlate pl) + a)

                      pPPlt = onPlate . head . filter ((== (x-1) `mod` (playersNum f)) . num) $ players f



---- 3 - SHELL DATA MANIPULATION FUNCTIONS -------------------------------------

    -- Add a new Frame to the State by providing only the fields which change
    -- with respect to the previous one
newFrame :: State -> [(String, FrameField)] -> State
newFrame s@(f:_) fieldList = (Frame act plN dea dCN tab mCs plt pls):s
    where act = newField action      toA "action"
          plN = newField playersNum  toI "playersNum"
          dea = newField dealer      toI "dealer"
          dCN = newField cardsInDeck toI "cardsInDeck"
          tab = newField table       toC "table"
          mCs = newField myCards     toC "myCards"
          plt = newField plate       toI "plate"
          pls = newField players     toP "players"

          newField :: (Frame -> a) -> (FrameField -> a) -> String -> a
          newField field extractor key =
                maybe (field f) extractor . M.lookup key $ M.fromList fieldList


    -- Take cards in shorthand as input, and if correct, execute a StartHand, Flop,
    -- Turn or River
cardHandler :: State -> Action -> [String] -> (State,String)
cardHandler s act sCs = maybe (s, "Cards have been mistyped") actFunc mCs
    where mCs = sequence $ map toCard sCs
          actFunc = case act of
                        StartHand -> (\cs -> (startHand s cs,
                                    "Starting hand added: " ++ (show cs)) )
                        Flop  -> (\cs -> (addCards s Flop cs,
                                    "Flop added: " ++ (show cs)) )
                        Turn  -> (\cs -> (addCards s Turn cs,
                                    "Turn added: " ++ (show cs)) )
                        River -> (\cs -> (addCards s River cs,
                                    "River added: " ++ (show cs)) )


    -- Read out any field of the current Frame
fieldHandler :: State -> String -> (State,String)
fieldHandler s@(f:_) funcStr = (s, msg)
    where msg = case funcStr of
            "action"      -> val action
            "playersNum"  -> val playersNum
            "dealer"      -> val dealer
            "cardsInDeck" -> val cardsInDeck
            "table"       -> val table
            "myCards"     -> val myCards
            "plate"       -> val plate
            "players"     -> val players
            _             -> "Field not recognised"

          val :: Show a => (Frame -> a) -> String
          val func = show $ func f



