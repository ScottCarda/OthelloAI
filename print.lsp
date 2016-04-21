#|
                       ***** PRINT.LSP *****

This file contains the print function to print the othello board to the
screen, as well as functions to convert the row and column between starting
at zero 1 starting at 1. 

Authors: Allison Bodvig, Scott Carda
Written Spring 2016 for CSC447/547 AI class.

|#

#|--------------------------------------------------------------------------|#
#|                             Print Board                                  |#
#|--------------------------------------------------------------------------|#


( defun printBoard ( game )
"Prints the othello board"

    ( let ( ( i 1 ) )
        ; format column numbers
        ( format t "  1 2 3 4 5 6 7 8~%" )
        ( dolist ( x game )
            ; print row number
            ( format t "~a " i )
            ( dolist ( y x )
                ; if null print - else print what is there
                ( if ( null y )
                    ( format t "- " )
                    ( format t "~a " y )
                )
            )
            ; increment row number
            ( setf i ( 1+ i ) )
            ( format t "~%" )
        )

        ( format t "~%" )
    )
)

#|--------------------------------------------------------------------------|#
#|                             XY To Output                                 |#
#|--------------------------------------------------------------------------|#


( defun xyToOutput ( pos ) 
"Returns row and column with 1 added to each"
    ; add one to row and column
    ( list ( 1+ ( cadr pos ) ) ( 1+ ( car pos ) ) )

)

#|--------------------------------------------------------------------------|#
#|                             Input to XY                                  |#
#|--------------------------------------------------------------------------|#

( defun inputToXY ( pos ) 
"Returns row and column with 1 subtracted from each"
    ; subtract one from row and column
    ( list ( 1- ( cadr pos ) ) ( 1- ( car pos ) ) )

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
