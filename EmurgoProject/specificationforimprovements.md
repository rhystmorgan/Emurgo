# Spec for Improvements of RPS Game

---

## UI

---

Build an interface for the game, 

The UI can also show the running score with the amount of games left

To display an Interface -- ANSI Terminal

The UI will be show as a grid, with relevent elements of the display being show in apropriate positions defined in the program

Text will display to cue next steps and print relevant results, update scores in the game and other necessary information (like the strategy being used by Croc?)

Strategy will print the strategy used for the previous round not the next round, this will just show the user what strategy is implemented at any time, to show if it is changing or updating etc

---

## UI Design 

Grid size: (61, 12)

---

| Rock Paper Scissors     | `Player` vs `Croc` | V. 0.2.0.0 |

Game: Best Of x                                 Scoreboard
Round: x                                        `Player`: x
Strategy: *Removed*                             `Croc`  : x

`First Line Print Instructions Here`
`Second Line for Print`
`Third Line for Print`

Controls:   | Rock      | Paper     | Scissors  |
            | [R]       | [P]       | [S]       |

---

## Strategies

---

### Random Strategy 

- random implementation of plays
- This will need to be implemented

- This will look something like this:

take x $ unfoldr (\g -> let a, g') = random @Int g in Just (mod a 3, g')) (mkStdGen z)

- where x is the number of plays required (bestOf x) and z is the (length of player's name)
- This will leave me with a list of random numbers between 0-2

- Slightly adjusted implementation in V2 where:

randomMove x = randomMove ((length (show y)) * z)
                          | Length of Move | C (Count of Plays inc Draws) |

randomMove :: Int -> Move
randomMove x = numMove $ randomList $ randomVal x

randomVal :: Int -> Int
randomVal x = fst $ random (mkStdGen x)

randomList :: Int -> Int
randomList x = randomVal x `mod` 3

### Reverse Strategy 

- play the user's moves back at them
- This will need to be implemented

- This look something like:
- Store single value from previous round and pass that into the next game
- Append `[Move]` with the latest `Move` using an index of `count - 1`

- Unable To implement in V2 as I was having lots of confilcts in types and expected types and didnt have the time to figure out a simple solution to the problem

### Name Strategy 

- Pick a move based on the name of the user (Already Implemented) This is the main function of `strategy` and creates part 1 of the programs strategy

- The new implementation of this will just be the player's name

### Beats Strategy 

- plays the move that would beat the user's previous play (Already Implemented)
- Implementation at the moment is part of the construction of the `strategy` defined as part 2 of the name based strategy

- The new implementation of this will just be `beatsStrat`

### Loses Strategy 

- plays the move that would lose to the user's previous play (Already Implemented)
- Implementation at the moment is part of the construction of the `strategy` defined as part 3 of the name based Strategy

- The new implementation of this will just be `loseStrat`

### Multiple Draws / Requiring Extra Moves

- Right now, we cover the need for extra moves by stringing multiple strategies together
- The need for more than a given number of moves is necessary for multiple games / more than three rounds / short names etc.
- The program will create a list of moves based on the given strategies and it will calculate plays based on the `count` and `length` of the `strategy`
- If the `count` goes above the `length` the program will deduct `length` from `count` and use this as the index for the `strategy`
- This will allow games to go on for ANY specified number of `rounds` without running out of moves

- V2 Implementation:
    I was unable to get the indexing to work effectively within the allotted time, so I have set the initial moves calculation to 2n ^ 2 * p

    this means the length of the initial set of moves will take the intial set from the players name, then multiply it by (2* number of Rounds) ^2

    This will create a 36 * 4 length list of Moves predefined (for a 4 letter name) meaning it would be practiacally impossible for the program to run out of moves in any given game. 

---

I have already started implementing this V2 of the project, and would like the chance to complete this implementation before being graded, if possible.

Based on the specification it should not take more than 2 days to implement, and so I would aim to have the Project V2 delivered by Wednesday 29th.

---
