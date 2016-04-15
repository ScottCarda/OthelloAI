#| 
    othello.lsp
|#

( load 'state )
( load 'generate )
( load 'print )
( load 'evaluations )
( load 'alpha-beta )

( load 'test )

( defun othello ( &optional player )

    ( let (
            ( input nil ) 
            playerStart
         )
        ( cond 
            
            ( ( null player )

                ( loop while 
                    ( and ( not ( eq input 'Y ) ) ( not ( eq input 'N ) ) ) do
                    ( princ "Would you like to move first [y/n]? " )
                    ( setf input ( read ) )
                )

                ( if ( eq input 'Y )
                    ( setf playerStart "black" )
                    ( setf playerStart "white" )
                )
            )

            ( ( eq player 'black )
                ( setf playerStart "black" )
            )

            ( ( eq player 'white )
                ( setf playerStart "white" )
            )
        )
        
        ( cond 
            ( ( string= playerStart "black" )
                ( format t "OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%" )
            )

            ( ( string= playerStart "white" )
                ( format t "OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%" )
            )    
        )

        ( setf player ( if ( string= playerStart "black" ) 'B 'W ) ) 

        ( wrapping-foo player )
    )
)

( defun player-move ( curState )

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

( defun take-turns ( user-color )
    ( let
        (
            ( ply 3 )
            ( curState ( get-start ) )
            ;( curState ( get-sample ) )
            ( turns-passed 0 )
        )
        
        ; Print the start state
        ( printBoard ( state-board curState ) )

        ( do ()
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
            ( if ( eq turns-passed 2 )
                ( format t "Game Over~%" )
            )
            
        )
    )
)

( defun must-pass? ( curState )
    ( zerop ( length ( state-moves curState ) ) )
)

( defun make-move ( position player ply )
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

( defun make-move-state ( curState ply )
    ( alpha-beta
        curState
        ply
        #'gen-successors
        #'evaluate
    )
)

( if ( = ( length *args* ) 1 )
    ( cond 
        ( ( string-equal ( car *args* ) "black" )
            ( othello 'black )
        )
        ( ( string-equal ( car *args* ) "white" ) 
            ( othello 'white )
        )
        ( t 
            (format t "Command Line Usage: clisp othello.lsp player (black or white)~%")
        )
    )

)
