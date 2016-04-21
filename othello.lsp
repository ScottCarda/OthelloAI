#|
                       ***** OTHELLO.LSP *****

This file contains the

Authors: Allison Bodvig, Scott Carda
Written Spring 2016 for CSC447/547 AI class.

|#

( load 'state )
( load 'generate )
( load 'print )
( load 'evaluations )
( load 'alpha-beta )

( load 'test )


#|--------------------------------------------------------------------------|#
#|                             Othello                                      |#
#|--------------------------------------------------------------------------|#

( defun othello ( &optional player )
"Allows the user to start the program from inside clisp"

    ( let (
            ( input nil ) 
            playerStart
         )
        ( cond 
            ; checks for no optional argument
            ( ( null player )
                ; asks user if they want to go first
                ( loop while 
                    ; loops until user enters valid option
                    ( and ( not ( eq input 'Y ) ) ( not ( eq input 'N ) ) ) do
                    ( princ "Would you like to move first [y/n]? " )
                    ( setf input ( read ) )
                )
                ; sets the player color based on if they want to move first
                ( if ( eq input 'Y )
                    ( setf playerStart "black" )
                    ( setf playerStart "white" )
                )
            )

            ; checks if user specfies black
            ( ( or ( eq player 'black ) ( eq player 'B ) )
                ( setf playerStart "black" )
            )
            ; checks if user specfies black
            ( ( or ( eq player 'white ) ( eq player 'W ) )
                ( setf playerStart "white" )
            )
            ; prints usage statement and sets player to nil since no vaid option was entered
            ( t 
                ( format t "Clisp Useage: ( othello [player] ) where player is either black or white. ~%" )
                ( setf playerStart nil )
            )
        )
        
        ; prints info based on what color the user is
        ( cond 
            ( ( string= playerStart "black" )
                ( format t "OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%" )
            )

            ( ( string= playerStart "white" )
                ( format t "OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%" )
            )    
        )

        ; checks that valid option was entered
        ( when ( not ( null playerStart ) ) 
            ( setf player ( if ( string= playerStart "black" ) 'B 'W ) ) 
            ; starts the game
            ( take-turns player )
        )
    )
)


#|--------------------------------------------------------------------------|#
#|                             Player Move                                  |#
#|--------------------------------------------------------------------------|#

( defun player-move ( curState )
"Gets move from player"
    ( let 
        (
            ( invalid t )
            row
            col
            posMoves
            newState
        )
        ( when
            ( setf posMoves ( state-moves curState ) )
            ; check until valid move
            ( do ()
                ; exit when valid move is entered
                ( ( null invalid ) newState )
                ( format t "What is your move [row col]? ")
                ; read in row and column
                ( setf row (1- ( read ) ) )
                ( setf col (1- ( read ) ) )
                ( format t "~%" )

                ; loop through possible moves
                ( dolist ( x posMoves )
                    ; check if row and col match acceptable move
                    ( when 
                        ( and (= row ( cadr ( first x ) ) ) (= col ( car (first x ) ) ) )
                        ( setf invalid nil )
                        ( setf newState ( move-to-state curState x ) )
                    )
                )
                ; prints possible moves if invalid move was entered
                ( when invalid
                    ( format t "~a~%" 
                        ( mapcar #'( lambda ( move ) 
                            ( xyToOutput ( first move ) ) 
                        ) posMoves ) 
                    )
                )
            )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                              Take Turns                                  |#
#|--------------------------------------------------------------------------|#

( defun take-turns ( user-color )
"Takes turns between computer and player to play game"

    ( let
        (
            ( ply 3 )
            ( curState ( get-start ) )
            ; ( curState ( get-sample ) )
            ( turns-passed 0 )
            pointsB
        )
        
        ; Print the start state
        ( printBoard ( state-board curState ) )

        ( do ()
            ; loops until both players cannot make a move
            ( ( >= turns-passed 2 ) nil )
            
            ( cond
            
                ; Player's turn
                ( ( eq user-color ( state-player curState ) )
                    ( cond
                    
                        ; If the player must pass
                        ( ( must-pass? curState )
                            ; Keep track of consecutive turns passed
                            ( incf turns-passed )
                            ; Perform a pass move ( changes whose turn it is in the state )
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
                            ; Perform a pass move ( changes whose turn it is in the state )
                            ( setf curState ( move-to-state curState nil ) )
                        )
                        
                        ; If the computer can make a move
                        ( t
                            ; If previously the player passed
                            ( if ( eq turns-passed 1 )
                                ( format t "You must pass.~%~%" )
                            )
                            ; Update the current state with the computer's move
                            ( setf curState ( make-move-state curState ply ) )
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
                ( format t "Game Over~%~%" )
                ( setf pointsB ( score ( state-board curState ) ) )
                ( cond 
                    ( ( > pointsB 32 ) 
                        ( format t "Black Wins!~%" )    
                    )
                    ( ( = 32 pointsB ) 
                        ( format t "Tie Game~%" )
                    )
                    ( t
                        ( format t "White Wins!~%" ) 
                    )
                )
                ( format t "Final Score: Black ~a White ~a~%~%" 
                    pointsB ( - 64 pointsB ) )
            )
        )
    )
)

; count number of atoms at any level in list
( defun score ( L )
    ( cond
        ( ( null L ) 0 )                       
        ( ( atom L ) 
            ( if ( eq 'B L ) 1 0 ) 
        )                       
        ( t 
            ( + ( score ( car L ) ) ( score ( cdr L ) ) ) 
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                           Must Pass                                      |#
#|--------------------------------------------------------------------------|#

( defun must-pass? ( curState )
"Retruns true/nil if there are any moves that can be made"
    ( zerop ( length ( state-moves curState ) ) )
)


#|--------------------------------------------------------------------------|#
#|                             Make Move                                    |#
#|--------------------------------------------------------------------------|#

( defun make-move ( position player ply )
"Return best possible move"
    ; finds best move
    ( xyToOutput
        ( state-creationMove ( make-move-state
            ( make-state
                :board position
                :player player
                :moves ( find-move position player )
                :creationMove nil
            )
            ply
        ) )
    )
)

#|--------------------------------------------------------------------------|#
#|                             Make Move State                              |#
#|--------------------------------------------------------------------------|#

( defun make-move-state ( curState ply )
"Calls alpha-beta"    
    ( alpha-beta
        curState
        ply
        #'gen-successors
        #'evaluate
    )
)

; checks for argument from command line
( if ( = ( length *args* ) 1 )
    
    ( cond 
        ; checks if user entered black or B
        ( ( or ( string= ( car *args* ) "black" ) ( string= ( car *args* ) "B" ) )
            ( othello 'black )
        )
        ; checks if user entered white or W
        ( ( or ( string= ( car *args* ) "white" ) ( string= ( car *args* ) "W" ) )
            ( othello 'white )
        )
        ; prints command line usage
        ( t 
            (format t "Command Line Usage: clisp othello.lsp player (black or white)~%")
        )
    )

)
