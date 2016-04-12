( defstruct state board player moves creationMove )

( defun get-start ()
    ( let
        (
            ( board
                '(
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil  W   B  nil nil nil )
                ( nil nil nil  B   W  nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                )
            )
            ( player 'B )
        )
	    ( make-state 
            :board board
            :player player
            :moves ( find-move board player )
            :creationMove nil
        )
    )
)
 
( defun get-sample ()
    ( let
        (
            ( board
#|
                '(
                (  B   B   B   B   B   B   B   B  )
                (  B   B   B   B   W   B   B   B  )
                (  B   B   B   B   B   B   B   B  )
                (  B   B   B   B   B   B   B   B  )
                (  B   B   B   B  nil  B   B   B  )
                (  B   B   B   B   B   B   B   B  )
                (  B   B   B   B   B   B   B   B  )
                (  B   B   B   B   B   B   B  nil )
                )
|#
                '(
                ( nil nil nil nil nil nil  W  nil )
                ( nil nil nil nil  B   W  nil nil )
                ( nil nil nil nil  B  nil nil nil )
                ( nil  W   W   B   B  nil nil nil )
                ( nil nil  B   B   B   B  nil nil )
                ( nil nil nil  W  nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                )
            )
            ( player 'W )
        )
	    ( make-state 
            :board board
            :player player
            :moves ( find-move board player )
            :creationMove '( 4 1 )
        )
    )
)

( defmacro at ( board X Y )
    `( nth ,X ( nth ,Y ,board ) )
)

( defun find-move ( board player )
    ; Looping througth the rows of the board
	( do
		(
			( y 0 ( 1+ y ) )

			( move-lst nil )
			( other-player ( if ( eq player 'B ) 'W 'B ) )
			
			move
		)
		( ( >= y 8 ) ( merge-path move-lst ) )
        ; Looping through the columns of the board
		( do
			(
				( x
					( position player ( nth y board ) )
					( position player ( nth y board ) :start ( 1+ x ) )
				)
			)
			( ( not x ) nil )

;			( format t "Found piece at ~D, ~D~%" x y )

			; For each neighbor of board[i][j]
			( do ( ( m -1 ( 1+ m ) ) ) ( ( >= m 2 ) nil )
			( do ( ( n -1 ( 1+ n ) ) ) ( ( >= n 2 ) nil )
			
			    ( when
			        ( and
			            ; Skip out-of-bounds positions
			            ( >= ( + x m ) 0 ) ( >= ( + y n ) 0 )
			            ; When a neighbor is the other player's piece
			            ( eq ( at board ( + x m ) ( + y n ) ) other-player )
		            )

;				    ( format t "Walked from ~D, ~D to ~D, ~D~%" x y ( + x m ) ( + y n ) )
                    ( setf move ( walk board ( + x m ) ( + y n ) m n ) )

                    ( if move 
                        ( setf move-lst ( cons move move-lst ) )
                    )
			    )
			    
			) )
		)
	)
)

( defun walk ( board x y m n )
	( do
		(
			( player ( at board x y ) )
			( stop nil )
			( return-pos nil )
            ( path nil)
		)
		( stop return-pos )
	
        ( setf path ( cons ( list x y )  path ) )
        ; ( format t "Path: ~a~%" path )

;		( format t "Walked from ~D, ~D to " x y )

		; Move current position acording to the direction
		( setf x ( + x m ) )
		( setf y ( + y n ) )

;		( format t "~D, ~D~%" x y )

		( cond

			; Out of bounds
			( ( or ( < x 0 ) ( >= x 8 ) ( < y 0 ) ( >= y 8 ) )
;				( format t "Out of Bounds~%" )
				( setf stop T )
			)

			; Another of the player's pieces
			( ( eq ( at board x y ) player ) )

			; Empty spot
			( ( not ( at board x y ) )
;				( format t "Found move at ~D, ~D~%" x y )
				( setf stop T )
				( setf return-pos ( list ( list x y ) path ) )
                ; ( format t "~a~%" return-pos )
			)

			; Other player's piece
			( t
;				( format t "Other player's piece~%" )
				( setf stop T )
			)

		)
	)
)

( defun merge-path ( moves ) 

    (let (
            ( return-lst nil )
            in-lst
            ( move-lst ( copy-list moves ) )
         )
        ( dolist ( move move-lst return-lst )
            ; checks if move is already in the return-lst
            ( setf in-lst ( car ( member move return-lst :test 
                #'( lambda ( m1 m2 ) ( equal ( car m1 ) ( car m2 ) ) 
                  ) 
            ) ) )

            ; ( format t "Return-lst: ~a~%" return-lst)

            ( cond 
                ( ( null in-lst ) 
                    ( setf return-lst ( cons move return-lst ) )
                )

                (t 
                    ; ( format t "Before: ~a~%" ( nth 1 in-lst ) )
                    ; ( format t "Move: ~a~%" ( nth 1 move ) ) 
                    ( setf ( nth 1 in-lst ) ( append ( nth 1 move ) ( nth 1 in-lst ) ) )
                    ; ( format t "After: ~a~%" ( nth 1 in-lst ) )

                )
            )
        )
    )
)
