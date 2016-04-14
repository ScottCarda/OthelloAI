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

        ( setf player ( if ( eq player "black" ) 'B 'W ) )

        ( printBoard ( state-board ( get-start ) ) )
    )
)

( defun wrapping-foo ( user-color )
    ( let
        (
            ( ply 3 )
            ( curState ( get-start ) )
            ( turns-passed 0 )
        )
        
        ( printBoard ( state-board ( get-start ) ) )
        
        ( do ()
            ( ( >= turns-passed 2 ) nil )
            
            ( cond
                ( ( eq user-player ( state-player curState ) )
                    ( setf curState ( player-move curState ) )
                )
                
                ( t
                    ( setf curState ( make-move-state curState ply ) )
                )
            )
            
            ( if curState
                ( setf turns-passed 0 )
                ( incf turns-passed )
            )
            
        )
    )
)

( defun make-move ( position player ply )
    ( mapcar #'1+
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
