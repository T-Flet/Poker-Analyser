---- Poker Analyser
--
--      Author:
--          Dr-Lord
--
--      Version:
--          0.27 - 18-19/04/2015
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
import HandTypeCheckers
import HandRankings
import HandCounters
--import Probabilities

import Data.List (sort, sortBy, groupBy, maximumBy, group, partition, find)
import Data.Function (on)



---- 1 - MAIN FUNCTIONS --------------------------------------------------------

main = do
    putStrLn "Poker Analyser Shell"
    putStrLn "(Write \"h\" for command help)"
    gameShell initialState



    -- PERHAPS THIS CAN BE DONE WITH foldM WITH THE STATE AS THE ACCUMULATOR
gameShell :: State -> IO ()
gameShell state = do
    cmd <- getLine
    if cmd == "q"
        then do
            putStrLn "Game Over"
            putStrLn $ "The players' balances are " ++ (show $ balances state)
            putStrLn $ "Players " ++ (show $ inTheLead state) ++ " win"
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
    -- Setting related commands start with s
        -- Set players number
    ('s':'p':'n':' ':n) ->
                (setPlayersNum s (read n :: Int),
                    "Players number set to " ++ n)
    -- Set user number
    ('s':'u':'n':' ':n) ->
                setUserNum s (read n :: Int)
    -- Set players' balance
    ('s':'p':'b':' ':n) ->
                (addPlayersBal s (read n :: Int),
                    "Players balance set to " ++ n)
        -- Player x is dealer (the actual player is the first in whichever direction)
    ('s':x:'d':_) ->
                setDealer s (read [x] :: Int)

    -- Player related commands start with p
        -- Player x Folds
    ('p':x:'f':_) ->
                plFolds s (read [x] :: Int)
        -- Player x Bets (or Raises, but reporting the bet) by amount
    ('p':x:'b':' ':a) ->
                plBets s Bet (read [x] :: Int) (read a :: Int)
        -- Player x Raises by amount
    ('p':x:'r':' ':a) ->
                plBets s Raise (read [x] :: Int) (read a :: Int)
        -- Player x reveals his Cards
    ('p':x:'c':' ':v1:s1:' ':v2:s2:_) ->
                plRevealsHand s (read [x] :: Int) [[v1,s1],[v2,s2]]

        -- Back one action
    "b" ->
                (tail s,
                    "Revoked last action: " ++ (show . action $ head s))

    -- Card related commands start with p
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
        -- History of frame Actions
    "fh" ->
                (s, "History: " ++ (show $ map action s))

        -- MAKE A COMMAND TO DISPLAY THE NUMBER OF ROUNDS (CALCULATE OR STORE IT)
    -- Analysis commands begin with a
        -- Analyse the player's hand
    "ah" ->
                (s, "Your hand is a " ++ show ht ++ " of " ++ show htf)
                    where (Hand ht htf _ _) = bestHandType . concat $ map (\f-> f $ head s) [table, myCards]

    "re" ->
                roundEnd s

    -- Other inputs
        -- The help string
    "h" ->
                (s, help)

        -- Nothing entered
    "" ->
                (s, "")

        -- Otherwise: not a recognised command
    _ ->
                (s, "Command not recognised")



---- 2 - SHELL DIRECT FUNCTIONS ------------------------------------------------

    -- Command help
help :: String
help = " \n\
\   -- Setting commands start with s \n\
\   spn <Int>       Set players number \n\
\   sun <Int>       Set user number \n\
\   spb <Int>       Set players balance \n\
\   s<Int>d         Player x is dealer (the actual player is the first in whichever direction) \n\
\ \n\
\   -- Player related commands start with p \n\
\   p<Int>f         Player x Folds \n\
\   p<Int>b <Int>   Player x Bets amount (or Raises, but reporting the bet) \n\
\   p<Int>r <Int>   Player x Raises by amount \n\
\ \n\
\   p<Int>c <value><suit>(x2)   Player x reveals his hand \n\
\ \n\
\   b               Back one action \n\
\ \n\
\   -- Card related commands start with c \n\
\   ci <value><suit>(x2)    Set initial hand \n\
\   cf <value><suit>(x3)    Flop \n\
\   ct <value><suit>        Turn \n\
\   cr <value><suit>        River \n\
\ \n\
\   -- Frame related commands begin with f \n\
\   ff <field>  Show any field from current frame \n\
\   fff         Show the full frame fields \n\
\   fb          Show players' balances \n\
\   fa          Number of actions or frames \n\
\   fh          History of frame Actions \n\
\ \n\
\   -- Analysis commands begin with a \n\
\   ah          Analyse the player's hand \n\
\ \n\
\   re          Round End \n\
\ \n\
\   h           This help string \n\
\ "

    -- Show players' balances
balances :: State -> [(Int,Int)]
balances (f:_) = foldr extractBal [] $ players f
    where extractBal pl bs = (num pl, balance pl):bs


    -- Determine the players currently in the lead
inTheLead :: State -> [(Int,Int)]
inTheLead = sortBy (compare `on` fst) . last . groupBy ((==) `on` snd) . sortBy (compare `on` snd) . balances


    -- Set the number of players
setPlayersNum :: State -> Int -> State
setPlayersNum s n = addFrame s [("action", FA (SetPlayers n)), ("playersNum", FI n), ("players", FP pls)]
    where pls = map initialPlayer [1..n]


    -- Set the user's Player number
setUserNum :: State -> Int -> (State,String)
setUserNum s@(f:_) n = case find ((== n) . num) $ players f of
    Nothing -> (s, "Non-existent player")
    Just p  -> (ns, "User number set to " ++ show n)
        where ns = addFrame s [("action", FA (SetUser n)), ("userNum", FI n)]


    -- Set the balance of all players
addPlayersBal :: State -> Int -> State
addPlayersBal s@(f:_) n = addFrame s [("action", FA (SetBalance n)), ("players", FP nPls)]
    where nPls = map setBal $ players f
          setBal p = newPlayer p [("balance", PI $ balance p + n)]


    -- Set the player x (x after the actual player) to be the dealer
setDealer :: State -> Int -> (State,String)
setDealer s@(f:_) x = case find ((== x) . num) $ players f of
    Nothing -> (s, "Non-existent player")
    Just p  -> (ns, "Player " ++ show x ++ " is dealer")
        where ns = addFrame s [("action", FA (SetDealer x)), ("dealer", FI x)]


    -- Give out two cards per player
startHand :: State -> [Card] -> State
startHand s@(f:_) ucs = addFrame s $ [("action", FA (StartHand ucs)), ("deck", FD nd)] ++ nUsrTabl
    where nd = newDeck (deck f) ucs
          nUsrTabl = updatePlayerAndTable f (userNum f) ucs []


    -- Add some cards to the table (Flop, Turn, River) and recalculate the user's Hand
addCards :: State -> ([Card] -> Action) -> [Card] -> State
addCards s@(f:_) act tcs = addFrame s $ [("action", FA nAct), ("deck", FD nd)] ++ nUsrTabl
    where nd = newDeck (deck f) tcs
          nAct = act tcs
          nUsrTabl = updatePlayerAndTable f (userNum f) [] tcs


    -- Player x Folds
plFolds :: State -> Int -> (State,String)
plFolds s@(f:_) x = case find ((== x) . num) $ players f of
    Nothing -> (s, "Non-existent player")
    Just p  -> (ns, "Player " ++ show x ++ " folded")
        where ns = addFrame s [("action", FA (Fold x)), ("players", FP nPls)]
              nPls = map plStatus (players f)
              plStatus pl
                | num pl == x = Player x (balance pl) (onPlate pl) (Fold x) (hisCards pl) (hisHand pl)
                | otherwise   = pl


    -- CHECK THAT THE PLAYER EXISTS
    -- INTRODUCE NEGATIVE BALANCE CHECKS SOMEWHERE
    -- ALSO, CHECK THAT A Bet IS >= THE PREVIOUS ONE
    -- Player x bets or raises by amount a
plBets :: State -> (Int -> Int -> Action) -> Int -> Int -> (State,String)
plBets s@(f:_) act x a = case find ((== x) . num) $ players f of
    Nothing -> (s, "Non-existent player")
    Just p  -> (ns, "Player " ++ show x ++ actStr ++ show a)
        where ns = addFrame s [("action", FA nAct), ("plate", FI nPlat), ("players", FP nPls)]
              nPls = map plStatus (players f)
              nAct = act x a
              nBal = (balance p) - a
              nPlt = (onPlate p) + a
              pPPlt = onPlate . head . filter ((== (x-1) `mod` (playersNum f)) . num) $ players f

              (nPlat, actStr) = case nAct of
                Bet   _ _ -> (plate f + a, " bet ")
                Raise _ _ -> (plate f + pPPlt + a, " raised ")

              plStatus pl = if num pl == x
                then case nAct of
                    Bet   _ _ -> Player x nBal nPlt nAct (hisCards pl) (hisHand pl)
                    Raise _ _ -> Player x (nBal-pPPlt) (nPlt+pPPlt) nAct (hisCards pl) (hisHand pl)
                else pl


    -- Player x reveals his hand
plRevealsHand :: State -> Int -> [String] -> (State,String)
plRevealsHand s@(f:_) x sCs = case find ((== x) . num) $ players f of
    Nothing -> (s, "Non-existent player")
    Just p  -> case sequence $ map toCard sCs of
        Nothing -> (s, "Cards have been mistyped")
        Just cs -> (addFrame s $ updatePlayerAndTable f x cs [], "Player " ++ show x ++ " reveals " ++ show cs)


    -- Determine the round winners, clear all hands and table of cards and of
    -- fiches (giving them to the winners), and, depending on how the game is
    -- played, either put them back into the deck or not.
    -- Mention the player(s) in the lead
roundEnd :: State -> (State,String)
roundEnd s@(f:_)
    | any (null . hisCards) inRoundPls = (s, "Some players still in game have not revealed their hand!")
    | otherwise                        = (ns, nsStr)
    where ns = addFrame s [("action", FA RoundEnd), ("cardsInDeck", FI 52), ("table", FC []), ("plate", FI 0), ("players", FP nPls)] -- EVENTUALLY ADD FATE OF CARDS HERE (BACK IN DECK OR NOT)
          nsStr = "Players' Hands: " ++ show justHands ++ "\nRound winner(s) and prize(s): " ++ show winnersPrizes ++ ".\nPlayer(s) in the lead: " ++ show (inTheLead ns)
          nPls = map givePrize $ players f
          givePrize p
            | p `elem` winners = newPlayer p [("balance", PI (prize + balance p)), ("onPlate", PI 0), ("status", PA $
            Won [num p] prize), ("hisCards", PC []), ("hisHand", PH $ Hand HighCard (HV Two) 0 [])]
            | otherwise        = newPlayer p [("onPlate", PI 0), ("hisCards", PC []), ("hisHand", PH $ Hand HighCard (HV Two) 0 [])]
          winnersPrizes = map (\p-> (num p,prize)) winners -- ADD SPLIT POTS ETC HERE!!!!!!!!!!
          prize = (plate f) `div` (length winners)
          winners = head . groupBy eqPlHands $ plsByHands
          justHands = map (\p-> (num p, (\h-> (hType h, hTField h)) $ hisHand p)) plsByHands
          plsByHands = reverse . sortBy cmpPlHands $ inRoundPls
          inRoundPls = filter inRound $ players f
          inRound p = case status p of
                Out  _ -> False
                Fold _ -> False
                _      -> True
          cmpPlHands = cmpHands `on` (cards . hisHand)
          eqPlHands  = eqHands  `on` (cards . hisHand)



---- 3 - SHELL DATA MANIPULATION FUNCTIONS -------------------------------------

    -- Take cards in shorthand as input, and if correct, execute a StartHand, Flop,
    -- Turn or River
cardHandler :: State -> ([Card] -> Action) -> [String] -> (State,String)
cardHandler s act sCs = maybe (s, "Cards have been mistyped") actFunc mCs
    where mCs = sequence $ map toCard sCs
          actFunc = case act [] of
                        StartHand _ -> (\cs -> (startHand s cs,
                                        "Starting hand added: " ++ (show cs)) )
                        Flop _      -> (\cs -> (addCards s Flop cs,
                                        "Flop added: " ++ (show cs)) )
                        Turn _      -> (\cs -> (addCards s Turn cs,
                                        "Turn added: " ++ (show cs)) )
                        River _     -> (\cs -> (addCards s River cs,
                                        "River added: " ++ (show cs)) )


    -- Read out any field of the current Frame
fieldHandler :: State -> String -> (State,String)
fieldHandler s@(f:_) funcStr = (s, msg)
    where msg = case funcStr of
            "action"      -> val action
            "playersNum"  -> val playersNum
            "userNum"     -> val userNum
            "dealer"      -> val dealer
            "deck"        -> val deck
            "table"       -> val table
            "plate"       -> val plate
            "players"     -> val players
            _             -> "Field not recognised"

          val :: Show a => (Frame -> a) -> String
          val func = show $ func f


    -- Update the table and the user's Hand given (or not, meaning possibly empty
    -- lists) his new cards and the table's new ones
updatePlayerAndTable :: Frame -> Int -> [Card] -> [Card] -> [(String, FrameField)]
updatePlayerAndTable f pNum pcs tcs = [("table", FC nTab), ("players", FP nPls)]
    where nTab = tcs ++ (table f)
          nPls = map setPlrCards $ players f
          setPlrCards p
            | num p == pNum = newPlayer p [("hisCards", PC nPcs), ("hisHand", PH $ bestHand (nPcs ++ nTab))]
            | otherwise          = p
                where nPcs = pcs ++ hisCards p
