At each turn, the player choses an action. An action is a column from 1-7 to drop your piece in. 
The change will be visualized by adding a X piece to the player's turn and O piece for the system turn.
The player chooses an action by providing the index (starting from 1) to standard input. 
The system chooses its action using alpha-beta approach. 

For efficiently check whether 4 tokens are connected, the board is represented as a (H+1)*W long bitstring.
Each bit correspond to a position on the board, starting from the bottom of each column and moving upwards. The first column is the left one.
We use two of these bitstrings: one to encode the position of the player, and another to encode the position of both the player and the computer (mask).
The positions of only the computer can then be quickly computed using the XOR-operator.
We add an extra row of bits to the array in order to clearly distinguish between the top and bottom rows. This helps prevent false positive when checking for vertical sequences.

This is how I check whether four tokens are connected horizontally:
1) shifting the bit-string 7 positions to the right and taking an AND-mask. 
    The new bitboard can be interpreted as follows: a bit is equal to 1 if there is a token to left of it and if it is the same 
    (i.e we can connect two tokens horizontally to the left from that position)
2) shifting the result 14 positions to the right and apply an AND-mask again.
    This checks whether we can match two horizontally consecutive tokens with two other horizontally consecutive tokens 2 positions left to it on the board. 
    The steps combined are equivalent to checking whether four tokens are connected horizontally. 
- The operations for the other directions are almost the same, by shifting by other amounts.

Each game tree node (game state) is scored as follows, assuming both sides play perfectly:
0 if the game ends in a draw
22 - a, if the current player can win
a - 22 if the opponent can win
where a is the number of pieces used by the winner at the end of the game

Since we cannot assign a score when the game has not finished, we explore the tree until we reach a leaf node and propagate this score back upwards to the root.
At each internal node, we choose the maximum value from its children. 

Since exploring the entire search tree is not feasible, I use alpha-beta approach.
Additionally, I applied a iterative deepening & null window search approach & hash table to store the upper bound for a given game state. Without it the search is still not feasible.

To start the project, write: 
cabal run

Resources: 
http://blog.gamesolver.org/solving-connect-four/02-test-protocol/
https://towardsdatascience.com/creating-the-perfect-connect-four-ai-bot-c165115557b0