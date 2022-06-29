# Emurgo Haskell Project - Write Up

My Haskell Project is a game of Rock Paper Scissors

In the game, the user plays against the computer, who picks moves by adjusting strategies, adapting to user input and the score of the game at any point.

Here is a video where I talk through the project, program logic and the decisions made to implement it.

https://youtu.be/Y889dymM6FI/

## The Basic Game

The program prints a UI into the terminal and cues the player for input when setting up the game:

- Player Name
- Number of Rounds

The instructions are written in the center of the interface and when the program takes inputs it updates the UI to include the information provided

Once the steps are complete the program uses the input to calculate a Basic Strategy for play. It creates a list of moves based on the name of the Player.

---

## Playing the Game

The game cue the player to input a Move : R | P | S

Then the display shows the player vs Croc (the name given to the computer generated opponent) with the relevant moves made by each person

The program then compares these values to declare a winner.

Once the winner is announced, the Scores and Rounds on the UI are updated and the program picks a new strategy for the next round, based on the initial list of Moves generated, and the Score and number of Plays made so far (this is to include Draws)

The strategy is then adjusted for the NEXT Move only, keeping the initial set list to adjust and iterate on in future rounds, and then it will play the game again for the next round until the game has finished.

Once the game is finished it declares a winner and exits the game leaving the UI on screen, printing the CLI prompt underneath the game UI.

---

## Strategies

There are several strategies that have been implemented which the computer will choose to play based on the current state of the game

Basic Strategy

This is based on the name of the user, where we assign a number to each letter and creates a list (2 * Rounds)^ 2 times the length of the player's name

This ensures the computer doesn't run out of moves to play in the case of multiple Draws happening over time.

Beat Strategy

This takes the Basic strategy and adjusts the next move according to what would beat that play

Lose Strategy

This takes the Basic Strategy and adjusts the next move according to what would lose to that play

Random

This take the length of the string of the next move and the count (amount of plays inc draws) and uses those values to calculate a random Move.

This is done so that the random move isn't the same every time, in fact it should be random every time because one or more of those given values will change.

Obviously the result is `mod` 3 so it will only ever be one of those 3.

---

