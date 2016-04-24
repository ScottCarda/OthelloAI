#|
                       ***** PRINT.LSP *****

    This file contains the print function to print the othello board to the
screen, as well as functions to convert the row and column between starting
at 0 and starting at 1.

Authors: Allison Bodvig
Written Spring 2016 for CSC447/547 AI class.

|#

#|--------------------------------------------------------------------------|#
#|                             Print Board                                  |#
#|--------------------------------------------------------------------------|#

; Prints the othello game board and the row and column numbers.
( defun printBoard ( game )
"Prints the othello board."

    ( let ( ( i 1 ) )
        ; Format column numbers
        ( format t "  1 2 3 4 5 6 7 8~%" )
        ( dolist ( x game )
            ; Print row number
            ( format t "~a " i )
            ( dolist ( y x )
                ; If null print - else print what is there
                ( if ( null y )
                    ( format t "- " )
                    ( format t "~a " y )
                )
            )
            ; Increment row number
            ( setf i ( 1+ i ) )
            ( format t "~%" )
        )

        ( format t "~%" )
    )
)

#|--------------------------------------------------------------------------|#
#|                          Input and Output                                |#
#|--------------------------------------------------------------------------|#

; Adds one to row and column.
( defun xyToOutput ( pos )
"Returns row and column with 1 added to each."
    ; Add one to row and column
    ( list ( 1+ ( cadr pos ) ) ( 1+ ( car pos ) ) )

)

; Subtracts one from row and column.
( defun inputToXY ( pos )
"Returns row and column with 1 subtracted from each."
    ; Subtract one from row and column
    ( list ( 1- ( cadr pos ) ) ( 1- ( car pos ) ) )

)
