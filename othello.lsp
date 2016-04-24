#|
                       ***** OTHELLO.LSP *****

    This program plays Othello using the AI technique of Alpha-Beta Minimax.
The program begins by asking the player if they would like to go first. Then
the game of Othello begins, and the start state of the board is printed. The
program will alternate turns between the AI and the player, each time printing
out the resulting board state. On the player's turn, they will be prompted to
enter in a board position for their move. If they enter an invalid position, a
list of valid moves will be displayed. On the AI's turn, the program will use
Alpha-Beta pruning to decide its move. The game will end when neither player
can make a move. At the end of the game, the program will tally the number of
coins each player has on the board, and the player with more coins wins. After
the game has concluded, the program will prompt the user if they would like to
play again.

The program can be started in several different ways. It may be started from
the command-line by giving othello.lsp and the color of the player that the
user wants to be as command-line arguments to the clisp interpreter. It may
also may be started by loading the othello.lsp file in the interpreter and
calling the othello function, which has an optional argument for the player
color the user wants to be. If this argument is not supplied, the program will
prompt the user for the color.

This file also contains the definitions for the othello-init function, which
was left empty for this project, and the make-move function, which can be used
in tournament play against other AIs. This function will take a board state,
a player color, and a ply-number, which is a measure of how deeply the AI
examines the state-space, and returns the position of the move that it thinks
is best for that player color.

Authors: Allison Bodvig, Scott Carda
Written Spring 2016 for CSC447/547 AI class.

|#

#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

( load 'alpha-beta )
( load 'state )
( load 'evaluations )
( load 'generate )
( load 'print )

#|--------------------------------------------------------------------------|#
#|                                 Othello                                  |#
#|--------------------------------------------------------------------------|#

; The calling function for playing Othello against an AI opponent.
; Handles processing of the optional player parameter.
( defun othello ( &optional player )
"Allows the user to play games of Othello against an AI opponent."
    ( cond

        ; If the optional player parameter was not given
        ( ( null player )
            ( repeat-game nil )
        )

        ; If the player parameter is black
        ( ( or ( eq player 'black ) ( eq player 'B ) )
            ( repeat-game 'B )
        )

        ; If the player parameter is white
        ( ( or ( eq player 'white ) ( eq player 'W ) )
            ( repeat-game 'W )
        )

        ; Prints usage statement
        ( t
            ( format t ( concatenate 'string
            "Clisp Usage: ( othello [player] ) where player is either "
            "black or white.~%" ) )
        )

    )
    ( values )
)

#|--------------------------------------------------------------------------|#
#|                               Game Methods                               |#
#|--------------------------------------------------------------------------|#

; Handles the end of game messages and final scores.
( defun game-over ( curState )
"Prints out the end game message along with the final scores."

    ( let
        (
            pointsB ; End-game score for Black
            pointsW ; End-game score for White
        )

        ( format t "Game Over~%~%" )
        ; Set number of points for black and white
        ( setf pointsB ( score ( state-board curState ) 'B ) )
        ( setf pointsW ( score ( state-board curState ) 'W ) )
        ( cond
            ; If black has more points
            ( ( > pointsB pointsW )
                ( format t "Black Wins!~%" )
            )
            ; If they are equal
            ( ( = pointsW pointsB )
                ( format t "Tie Game~%" )
            )
            ; Otherwise white is the winner
            ( t
                ( format t "White Wins!~%" )
            )
        )
        ; Prints final score
        ( format t "Final Score: Black ~a White ~a~%~%"
            pointsB pointsW )
    )
)

; Checks if the current player must pass the turn.
( defun must-pass? ( curState )
"Returns true if the current player has no moves to make and must pass."
    ( zerop ( length ( state-moves curState ) ) )
)

; Determines what color the player is in each game and loops the game logic.
( defun repeat-game ( player )
"Loops the game logic and handles logic for things at the beginning of a game."
    ; Repeats the game logic if the user wants to play multiple times
    ( do
        (
            input ; User input for prompts
        )
        ( ( eq input 'N ) ( values ) )

        ; When the player's color has yet to be decided
        ( when ( null player )

            ( setf input nil )
            ; Loop asking if player wants to go first until correct response
            ( do ()
                ( ( or ( eq input 'Y ) ( eq input 'N ) ) nil )
                ( format t "Would you like to move first [y/n]? " )
                ( setf input ( read ) )
            )

            ; Sets the player color based on if they want to move first
            ( if ( eq input 'Y )
                ( setf player 'B )
                ( setf player 'W )
            )
        )

        ; Prints info based on what color the user is
        ( cond

            ; If player is black
            ( ( eq player 'B )
                ( format t ( concatenate 'string
                "OK! You will be playing Black. When asked for your move, "
                "please enter the row and column in which you would like to "
                "place a Black stone. Remember, you must outflank at least "
                "one White stone, or forfeit your move.~%~%" ) )
            )

            ; If player is white
            ( ( eq player 'W )
                ( format t ( concatenate 'string
                "OK! You will be playing White. When asked for your move, "
                "please enter the row and column in which you would like to "
                "place a White stone. Remember, you must outflank at least "
                "one Black stone, or forfeit your move.~%~%" ) )
            )

        )

        ;( setf player ( if ( string= player "black" ) 'B 'W ) )
        ; Player's start taking turns
        ( take-turns player )
        ; Clear player for next game
        ( setf player nil )

        ( setf input nil )
        ; Loop asking if player wants to play again and until correct response
        ( do ()
            ( ( or ( eq input 'Y ) ( eq input 'N ) ) nil )
            ( format t "Would you like to play again? [y/n]? " )
            ( setf input ( read ) )
        )
    )
)

; Counts the number of coins for the given color.
; Returns the count.
( defun score ( L color )
"Returns the number of coins on the board owned by the given color."
    ( cond
        ; Check for empty list
        ( ( null L ) 0 )
        ; Check if atom
        ( ( atom L )
            ; Check if matches color, if so add one, else 0
            ( if ( eq color L ) 1 0 )
        )
        ( t
            ; Total score is car + cdr of list
            ( + ( score ( car L ) color ) ( score ( cdr L ) color ) )
        )
    )
)

; Handles taking turns between player and computer.
; Also checks for end game and prints winner/final score.
( defun take-turns ( user-color )
"Takes turns between computer and player to play the game."

    ( let
        (
            ; The depth into the state space the AI considers, hard-coded
            ( ply 3 )
            ( curState ( get-start ) ) ; The game state
            ( turns-passed 0 ) ; The number of turns passed in succession
        )

        ; Print the start state
        ( printBoard ( state-board curState ) )

        ; Turn loop
        ( do ()
            ; Loops until both players cannot make a move
            ( ( >= turns-passed 2 ) nil )

            ( cond

                ; Player's turn
                ( ( eq user-color ( state-player curState ) )
                    ( cond

                        ; If the player must pass
                        ( ( must-pass? curState )
                            ; Keep track of consecutive turns passed
                            ( incf turns-passed )
                            ; Perform a pass move
                            ; Changes whose turn it is in the state
                            ( setf curState ( move-to-state curState nil ) )
                        )

                        ; If the player can make a move
                        ( t
                            ; If previously the computer passed
                            ( if ( eq turns-passed 1 )
                                ( format t "I must pass.~%~%" )
                            )

                            ; Update the current state with the player's move
                            ( setf curState ( player-move curState ) )
                            ; Print the resulting state
                            ( printBoard ( state-board curState ) )
                            ; Reset number of consecutive turns passed
                            ( setf turns-passed 0 )
                        )
                    )
                )

                ; Computer's turn
                ( t
                    ( cond

                        ; If the computer must pass
                        ( ( must-pass? curState )
                            ; Keep track of consecutive turns passed
                            ( incf turns-passed )
                            ; Perform a pass move
                            ; Changes whose turn it is in the state
                            ( setf curState ( move-to-state curState nil ) )
                        )

                        ; If the computer can make a move
                        ( t
                            ; If previously the player passed
                            ( if ( eq turns-passed 1 )
                                ( format t "You must pass.~%~%" )
                            )
                            ; Update the current state with the computer's move
                            ( setf curState ( computer-move curState ply ) )
                            ; Print out the computer's move
                            ( format t "Here is my move: ~{~a ~} ~%~%"
                                ( xyToOutput ( state-creationMove curState ) )
                            )
                            ; Print the resulting state
                            ( printBoard ( state-board curState ) )
                            ; Reset number of consecutive turns passed
                            ( setf turns-passed 0 )
                        )
                    )
                )

            )

            ; If both the players pass, its game over
            ( when ( eq turns-passed 2 )
                ( game-over curState )
            )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                               Turn Methods                               |#
#|--------------------------------------------------------------------------|#

; Represents the actions of a computer's turn.
; Calls alpha-beta with the current state.
( defun computer-move ( curState ply )
"Calls alpha-beta to produce new game state."
    ( alpha-beta
        curState
        ply
        #'gen-successors
        #'evaluate
    )
)

; Asks the player for their move and checks if it is a valid move.
; Makes move and returns new state.
; Prints possible moves if player inputs wrong move.
( defun player-move ( curState )
"Performs a move by prompting the user for a position. Returns the resulting state."
    ( let
        (
            ( invalid t ) ; Boolean for if the entered move is invalid
            row ; The entered row of a move
            col ; The entered column of a move
            posMoves ; List of possible moves to make
            newState ; The game state created by the entered move
        )

        ; Prompt the user for a move only if there are moves to be made
        ( when ( setf posMoves ( state-moves curState ) )

            ; Check until valid move
            ( do ()
                ; Exit when valid move is entered
                ( ( null invalid ) newState )

                ( format t "What is your move [row col]? " )
                ; Read in row and column
                ( setf row ( 1- ( read ) ) )
                ( setf col ( 1- ( read ) ) )
                ( format t "~%" )

                ; Loop through possible moves
                ( dolist ( x posMoves )
                    ; Check if row and col match acceptable move
                    ( when
                        ( and
                            ( = row ( cadr ( first x ) ) )
                            ( = col ( car ( first x ) ) )
                        )
                        ( setf invalid nil )
                        ( setf newState ( move-to-state curState x ) )
                    )
                )
                ; Prints possible moves if invalid move was entered
                ( when invalid
                    ( format t "Possible Moves:~%~{~{~a ~}~%~}~%"
                        ( sort
                            ; Convert the moves to output format
                            ( mapcar #'( lambda ( move )
                                ( xyToOutput ( first move ) )
                            ) posMoves )
                            ; Sort the moves by row then column
                            #'( lambda ( x y )
                                ( if ( eq ( car x ) ( car y ) )
                                    ( < ( cadr x ) ( cadr y ) )
                                    ( < ( car x ) ( car y ) )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                            Tournament Methods                            |#
#|--------------------------------------------------------------------------|#

; Converts a board as a one dimensional list of elements to a board as
; a list of rows, each row being a list containing the elements of the row.
( defun convert-board ( board )
"Converts expected board format to the format used by this program."

    ; Replace all characters that are not W or B with NIL
    ( setf board
        ( mapcar
            #'( lambda ( elem )
                ( if ( or ( eq elem 'W ) ( eq elem 'B ) )
                    elem
                    nil
                )
            )
            board
        )
    )

    ; Partition board into list of rows
    ; Loop through each row of the board
    ( do
        (
            ( i 0 ( + 8 i ) ) ; Loop variable
            ( new-board nil ) ; The board as a list of rows
        )
        ; Stop when all the rows have been processed
        ( ( >= i ( length board ) ) new-board )

        ; Add the row to the end of the new board
        ( setf new-board
            ( append
                new-board
                ; The row as a list
                ( list ( subseq board i ( + 8 i ) ) )
            )
        )
    )
)

; Returns best possible move based on a give position, player and ply.
; The position is expected to be a one-dimensional list of elements
; of the board in row-major order.
( defun make-move ( position player ply )
"Return best possible move."
    ( let
        (
            ; Convert the expected format to the format
            ; use in this program
            ( board ( convert-board position ) )
            curState ; The current game state
        )

        ; Create a state based on the arguments
        ( setf curState
            ( make-state
                :board board
                :player player
                :moves ( find-move board player )
                :creationMove nil
            )
        )

        ; Return nil if you must pass
        ( unless ( must-pass? curState )

            ; Finds best move
            ( xyToOutput
                ( state-creationMove ( computer-move
                    curState
                    ply
                ) )
            )

        )
    )
)

; Empty init function provided for tournament play.
( defun othello-init ( )

)

#|--------------------------------------------------------------------------|#
#|                            Command Line Input                            |#
#|--------------------------------------------------------------------------|#

; Checks for argument from command line
( if ( = ( length *args* ) 1 )

    ( cond
        ; Checks if user entered black or B
        (
            ( or
                ( string-equal ( car *args* ) "black" )
                ( string-equal ( car *args* ) "B" )
            )
            ( othello 'B )
        )
        ; Checks if user entered white or W
        (
            ( or
                ( string-equal ( car *args* ) "white" )
                ( string-equal ( car *args* ) "W" )
            )
            ( othello 'W )
        )
        ; Prints command line usage
        ( t
            (format t
            "Command Line Usage: clisp othello.lsp player (black or white)~%")
        )
    )

)
