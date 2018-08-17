Clanki
======

Command-line spaced-repetition learning software. CL (command line) + Anki (popular spaced-repetition software) = Clanki. 

## Usage

Usage is fairly simple, just follow the instructions after running the program. Add a deck, add cards to the deck, then quiz whenever possible. The program will determine what cards need to be reviewed, using the Super Memo 2 algorithm. Here's an example :

![Screenshot](https://s3.amazonaws.com/f.cl.ly/items/1Q432s0y0H153J3m3D2m/Screen%20Shot%202015-01-20%20at%206.57.58%20PM.png)

## Quizzing

Quizzing is straight-forward, just run Clanki and select quiz from the menu, and Clanki will decide what cards you should review. There will be a question, and then it will ask for your answer. After you type your answer, you're asked to rate your answer, on a scale from 1 to 5. 1 is a complete blackout, and 5 is immediate recall.

## Installation

As of right now, Clanki can only be installed through cabal, which is part of the haskell-platform. If you already have the haskell-platform, just run the following :

    cabal update
    cabal install clanki

The clanki executable should now be available in `$HOME/.cabal/bin/`.

## Command-line options

Clanki accepts argument when run, here are some examples :

Quiz on 2 cards from the deck 'derivatives' :
    
    clanki derivatives 2

Quiz on all cards in the deck 'derivatives' :
   
    clanki derivatives

List all decks :
    
    clanki --list 'or' clanki -l

Display help :

    clanki --help 'or' clanki -h

## Help wanted

If anyone reading this knows a bit about homebrew, I'd welcome a pull request, or some pointers on how to get my package on there. 
