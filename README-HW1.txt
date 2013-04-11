## State
I used a vector of vectors to represent my board.
Pieces are represented by the same characters as in the wiki.
The state is modeled by a map that contains the fields board, turn, and on-move.

## Move generation
Move scanning happens mostly the same as in the demo pseudocode.
Instead of using a stop-short argument though I have a maximum manhatten distance argument
that defaults to the length + the width of the board.

For actual move generation I have a "mover" function that maps an action function over a sequence of directions
and builds a set from the union of all sets in this sequence.
The action function that is passed is a function that performs the actual movescan of the board at the
current coordinates, along with capture/no capture maximum manhatten distance.
Each pieces special move generation lives in a multimethod that specializes on piece class.

## Moves
Move application is implemented in a funtion called apply-move, it returns a new gamestate updated with the move given applied.
My human-move function takes a state as an argument and attempts to prompt for and read a move from \*out\*/\*in\*,
if an invalid move is read in human-move will alert and recursively call itself until a valid move is read in.
