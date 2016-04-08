#| 
    othello.lsp
|#

(load 'print.lsp)



(defun othello (&optional player)

    (let (
            (input nil) 
            (playerStart)
         )
        (cond 
            
            ((null player)

                (loop while (and (not (eq input 'Y)) (not (eq input 'N))) do
                    (princ "Would you like to move first [y/n]? ")
                    (setf input (read))
                )

                (if (eq input 'Y)
                    (setf playerStart "black")
                    (setf playerStart "white")
                )
            )

            ((eq player 'black)
                (setf playerStart "black")
            )

            ((eq player 'white)
                (setf playerStart "white")
            )
        )
        
        (cond 
            ((string= playerStart "black")
                (format t "OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%")
            )

            ((string= playerStart "white")
                (format t "OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%")
            )    
        )

        (printBoard (get-start))
    )
)


#|(setf game '((nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil W B nil nil nil) 
             (nil nil nil B W nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil)
             ))
|#

( defstruct state board player )

#| ( defun get-start ()
	( make-state 
        :board '(
    		( nil nil nil nil nil nil nil nil )
            ( nil nil nil nil nil nil nil nil )
            ( nil nil nil nil nil nil nil nil )
            ( nil nil nil  W   B  nil nil nil )
            ( nil nil nil  B   W  nil nil nil )
            ( nil nil nil nil nil nil nil nil )
            ( nil nil nil nil nil nil nil nil )
            ( nil nil nil nil nil nil nil nil )
	    )
        :player 'B
    )
) |#

( defun get-start ( ) 

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

( defun get-sample ()
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

#|( defun get-sample ()
	'(
        ( 0  1  2  3  4  5  6  7  )
        ( 8  9  10 11 12 13 14 15 )
        ( 16 17 18 19 20 21 22 23 )
        ( 24 25 26 27 28 29 30 31 )
        ( 32 33 34 35 36 37 38 39 )
        ( 40 41 42 43 44 45 46 47 )
        ( 48 49 50 51 52 53 54 55 )
        ( 56 57 58 59 60 61 62 63 )
	)
)|#


#|( defmacro at ( state X Y )
	`( if ( and ( >= ,X 0 ) ( >= ,Y 0 ) )
		( nth ,X ( nth ,Y ,state ) )
	)
)|#

( defmacro at ( state X Y )
    `( nth ,X ( nth ,Y ,state ) )
)

( defun test ( state player )
    ( let
        (
            ( successors ( gen-successors state player ) )
            ( succ nil )
        )
        
        ( setf succ ( funcall successors ) )
        
        ( do ()
            ( ( not succ ) nil )
            
            ( printBoard succ )
            
            ( setf succ ( funcall successors ) )
            
        )
        
        ;( mapcar #'printBoard ( gen-successors state 'B ) )
    )
    ( values )
)

( defun move-to-state ( state move player ) 
    ( let  (
            ( path ( cadr move ) )
            ( pos ( car move ) )
            newState
          )

        ( setf newState ( flip-tiles state path ) )

        ( setf
            ( at
                newState 
                ( car pos ) 
                ( cadr pos )
            )
            player
        ) 
        newState
    )
)

#|
( defun gen-successors ( state player ) 

    ( mapcar #'( lambda ( move ) ( move-to-state state move player ) )
        ( find-move state player )
    )
)
|#

( defun gen-successors ( state player ) 
    ( let
        (
            ( move-lst ( find-move state player ) )
            ( i -1 )
            max-i
        )
        
        ( setf max-i ( - ( length move-lst ) 1 ) )
        
        ( lambda ()
            ( when ( < i max-i )
                ( incf i )
                ( move-to-state state ( nth i move-lst ) player )
            )
        )
            ; ( find-move state player )
    )
)

( defun find-move ( state player )
    ; Looping througth the rows of the state
	( do
		(
			( y 0 ( 1+ y ) )

			( succ-lst nil )
			( other-player ( if ( eq player 'W ) 'B 'W ) )
		)
		( ( >= y 8 ) ( merge-path succ-lst ) )
        ; Looping through the columns of the state
		( do
			(
				( x
					( position player ( nth y state ) )
					( position player ( nth y state ) :start ( 1+ x ) )
				)
			)
			( ( not x ) nil )

		;( setf x_coord ( position player ( nth y_coord state ) :start ( 1+ x_coord ) ) )

		; Piece found @ state[x_coord][y_coord]
		;( when x_coord

			( format t "Found piece at ~D, ~D~%" x y )

			; For each neighbor of state[i][j]
			( do ( ( m -1 ( 1+ m ) ) ) ( ( >= m 2 ) nil )
			( do ( ( n -1 ( 1+ n ) ) ) ( ( >= n 2 ) nil )
			
			    ( when
			        ( and
			            ;( not ( and ( zerop m ) ( zerop n ) ) )
			            ; Skip out-of-bounds positions
			            ( >= ( + x m ) 0 ) ( >= ( + y n ) 0 )
			            ; When a neighbor is the other player's piece
			            ( eq ( at state ( + x m ) ( + y n ) ) other-player )
		            )

				    ( format t "Walked from ~D, ~D to ~D, ~D~%" x y ( + x m ) ( + y n ) )
                    (setf err ( walk state ( + x m ) ( + y n ) m n ) )

                    (if err 
                        ( setf succ-lst ( cons err succ-lst ) )
                    )
				    ;( format t "There~%" )
			    )
			    
			) )
		)
	)
)

( defun walk ( state x y m n )
	( do
		(
			( player ( at state x y ) )
			( stop nil )
			( return-pos nil )
            ( path nil)
		)
		( stop return-pos )
	
        ( setf path ( cons ( list x y )  path ) )
        ; ( format t "Path: ~a~%" path )

		( format t "Walked from ~D, ~D to " x y )

		; Move current position acording to the direction
		( setf x ( + x m ) )
		( setf y ( + y n ) )

		( format t "~D, ~D~%" x y )

		( cond

			; Out of bounds
			( ( or ( < x 0 ) ( >= x 8 ) ( < y 0 ) ( >= y 8 ) )
				( format t "Out of Bounds~%" )
				( setf stop T )
			)

			; Another of the player's pieces
			( ( eq ( at state x y ) player ) )

			; Empty spot
			( ( not ( at state x y ) )
				( format t "Found move at ~D, ~D~%" x y )
				( setf stop T )
				( setf return-pos ( list ( list x y ) path ) )
                ; ( format t "~a~%" return-pos )
			)

			; Other player's piece
			( t
				( format t "Other player's piece~%" )
				( setf stop T )
			)

		)
	)
)

( defun merge-path ( succ-lst ) 

    (let (
            ( return-lst nil )
            in-lst
            ( move-lst ( copy-list succ-lst ) )
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


( defun flip-tiles ( state path ) 

    ( let (
            player
            ( newState ( mapcar #'copy-list state ) )
          )

        ( dolist ( tile path newState ) 
            ( setf player ( at newState ( car tile ) ( cadr tile ) ) )

            ( setf player ( if ( eq player 'W ) 'B 'W ) )

            ;( setf ( nth ( car tile ) ( nth ( cadr tile ) newState ) ) player )
            ( setf ( at newState ( car tile ) ( cadr tile ) ) player )
        )
    )
)
