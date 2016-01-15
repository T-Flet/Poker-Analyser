Poker Analyser
===

INCOMPLETE and not really user friendly. I might pick it up again in the future;
at present the returned probabilities are not always accurate (they diverge from
the real ones in some conditions). It was a very fun project to code up.

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


## Main Abstract Algorithm

Very basically, whenever the player asks for the probability of something, this
is what happens (only the steps needed by the specific request are executed):
- Everything is checked for changes since the last request and the following recalculated if there are any;
- The Table Cards are analysed and their Hand ranked absolutely (for the purposes of the other players’ chances), then the Player Cards are added and the same procedure is repeated;
- Then the number of better Hands which can exist at that stage in the game are calculated for both sets, which means cleverly subtracting amounts from the total number of better Hands;
- Then the specific request is answered: it could be what the best Hand the player can get at that turn is, or simply (not in the least) what the probability of winning that round is.



## Files in the project:

    Analyser.hs : The main program, containing the shell and calling on all others.

    GeneralFunctions.hs : A package of some of my general functions, useful in
            many different situations.

    DataTypes.hs : Contains all Data Types declarations Type Class instances and
            general Type-related functions.

    HandTypeCheckers.hs : Contains all the functions which test lists of cards
            in order to determine whether they constitute Hands.

    HandRankings.hs : Contains functions related to counting and ranking sets of
            5 cards (hands) among all the possible ones.

    Probabilities.hs : DEVELOPMENT PAUSED Contains all the functions related to
            determining probabilities: of the player or others having a certain
            hand, of getting a certain hand, of beating a certain hand, etc.

    DoAndConsider.txt : Contains all things that are going to be done and those
            under consideration, along with some notes on the program and some
            very useful test data.

    README.md : This file.
