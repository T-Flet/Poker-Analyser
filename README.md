Poker Analyser
===

INCOMPLETE. I might pick it up again in the future; at present the returned
probabilities are not always accurate (they diverge from the real ones in some
conditions). It was a very fun project to code up.

===

Poker analysing shell, which provides game tracking with minimal input and, on
request of the user, detailed probabilities of specific hands and outcomes.

    Inupts:
        Player's cards, table cards (later on: player's fiches and even
        later other players' as well).
        Possible user request for (his or other's) probability of a
        specific hand.

    Outputs:
        Probabilty of player's or others' specific hand.
        Specific request response.
        (Later: suggested bet)


Files in project:

    Analyser.hs : The main program, containing the shell and calling on all others.

    GeneralFunctions.hs : A package of some of my general functions, useful in
            many different situations.

    DataTypes.hs : Contains all Data Types declarations Type Class instances and
            general Type-related functions.

    HandTypeCheckers.hs : Contains all the functions which test lists of cards
            in order to determine whether they constitute Hands.

    HandRankings.hs : Contains functions related to counting and ranking sets of
            5 cards (hands) among all the possible ones.

    Probabilities.hs : Contains all the functions related to determining
            probabilities: of the player or others having a certain hand, of
            getting a certain hand, of beating a certain hand, etc.

    DoAndConsider.txt : Contains all things that are going to be done and those
            under consideration, along with some notes on the program and some
            very useful test data.

    README.md : This file.
