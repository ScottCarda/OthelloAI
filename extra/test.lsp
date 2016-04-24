( defun test2 ()
    ( let
        (
            s
            move
            ( board ( state-board ( get-start ) ) )
            ( player 'B )
            ( input 'y )
        )
        
        ( printboard board )
        
        ( do ()
            ( nil ( values ) )

            ( setf s ( make-move-state board player 3 ) )
            ( cond
            
                ( s
                    ( setf move ( state-creationMove s ) )
                    ( setf board ( state-board s ) )
                    ( format t "~%~A: ~A~%" player ( mapcar #'1+ move ) )
                    ( printboard board )
                    
                )
                
                ( t
                    ( format t "Pass~%" )
                )
            )
            
            ( setf player ( if ( eq player 'B ) 'W 'B ) )
            
            ( setf input ( read ) )
            
        )
        
        #|
        ( setf s ( make-move-state board 'W 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "W: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        
        ( setf s ( make-move-state board 'B 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "B: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        
        ( setf s ( make-move-state board 'W 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "W: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        |#
    )
)

( defun test ( curState )
    ( let
        (
            ( successors ( gen-successors curState ) )
            ( succ nil )
        )
        
        ( setf succ ( funcall successors ) )
        
        ( do ()
            ( ( not succ ) nil )
            
            ( printBoard ( state-board succ ) )
            
            ( setf succ ( funcall successors ) )
            
        )
        
        ;( mapcar #'printBoard ( gen-successors state 'B ) )
    )
    ( values )
)

#|
; Takes in any arguments given by player
; if no arguments asks player what color they want to be
; prints statement based on what color the player is and starts the game
( defun othello ( &optional player )
"Allows the user to start the program from inside clisp"
    
    ; Loop until player doesnt want to play again
    ( do
        (
            ( input nil )
            playerStart
            ( again nil )
        )
        ( ( eq again 'N ) ( values ) )
        
        ; Get color of player
        ( cond
        
            ; Checks for no optional argument
            ( ( null player )
            
                ; Loop asking if player wants to go first until correct response
                ( do () 
                    ( ( or ( eq input 'Y ) ( eq input 'N ) ) nil )
                    ( format t "Would you like to move first [y/n]? " )
                    ( setf input ( read ) )
                )
                
                ; Sets the player color based on if they want to move first
                ( if ( eq input 'Y )
                    ( setf playerStart "black" )
                    ( setf playerStart "white" )
                )
            )

            ; Checks if user specfies black
            ( ( or ( eq player 'black ) ( eq player 'B ) )
                ( setf playerStart "black" )
            )
            ; Checks if user specfies white
            ( ( or ( eq player 'white ) ( eq player 'W ) )
                ( setf playerStart "white" )
            )
            ; Prints usage statement and sets player to nil since no vaid option was entered
            ( t 
                ( format t "Clisp Useage: ( othello [player] ) where player is either \"black\" or \"white\".~%" )
                ( setf playerStart nil )
            )
        )
        
        ; Prints info based on what color the user is
        ( cond

            ; If player is black
            ( ( string= playerStart "black" )
                ( format t "OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%" )
            )

            ; If player is white
            ( ( string= playerStart "white" )
                ( format t "OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%" )
            )
            
        )

        ; Checks that valid option was entered
        ( when ( not ( null playerStart ) ) 
            ( setf player ( if ( string= playerStart "black" ) 'B 'W ) ) 
            ; Starts the game
            ( take-turns player )
        )

        ; Reset again
        ( setf again nil )

        ; Loop asking if player wants to play again until correct response
        ( do ()
            ( ( or ( eq again 'Y ) ( eq again 'N ) ) )
            ( princ "Would you like to play again? [y/n]? " )
            ( setf again ( read ) )
        )
        
        ; Resets some values
        ( setf player nil )
        ( setf input nil )

    )
)



|#
