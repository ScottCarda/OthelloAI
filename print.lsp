#| 
    Print Function for Othello Program

|#

;( load 'heuristics )

( defun printBoard ( game )

    ( let ( ( i 1 ) )
        ( format t "  1 2 3 4 5 6 7 8~%" )
        ( dolist ( x game )
            ( format t "~a " i )
            ( dolist ( y x )
                ( if ( null y )
                    ( format t "- " )
                    ( format t "~a " y )
                )
            )
            ( setf i ( 1+ i ) )
            ( format t "~%" )
        )

        ( format t "~%" )
    )
)

#|( defun statePrint ( curState )
    ( when ( state-p curState )
        ( printBoard ( state-board curState ) )
        ( format t "Coin:    ~D~%" ( coin curState ) )
        ( format t "Corners: ~D~%" ( corners curState ) )
        ( format t "Moves:   ~D~%" ( moves curState ) )
        ;( format t "Stability: ~D~%" ( stability curState ) )
    )
)|#

; (printBoard game)
