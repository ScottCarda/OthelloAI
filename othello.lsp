#| 
    othello.lsp
|#

(load 'print.lsp)



(defun othello ()

    (let ((input) (move))
        (princ "Would you like to move first [y/n]? ")
        (setf input (read))

        (cond

            ((string= input "Y")
                (format t "OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%")
            )
            ((string= input "N")
                (format t "OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%")
            )

            (t
                (format t "Please enter either y or n.~%")
                (othello)
            )
        )
        (printBoard game)
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

( defun get-start ()
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
        ( nil nil  W   B  nil nil nil nil )
        ( nil nil  W   W   W  nil nil nil )
        ( nil nil nil  B   W   B  nil nil )
        ( nil nil  W   B   B  nil nil nil )
        ( nil nil nil  B   B   B  nil nil )
        ( nil nil nil nil nil nil nil nil )
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


( defmacro at ( state X Y )
	`( if ( and ( >= ,X 0 ) ( >= ,Y 0 ) )
		( nth ,X ( nth ,Y ,state ) )
	)
)

( defun find-move ( state player )
	( do
		(
			( y 0 ( 1+ y ) )

			( succ-lst nil )
			( other-player ( if ( eq player 'W ) 'B 'W ) )
		)
		( ( >= y 8 ) ( remove-duplicates succ-lst :test #'equal ) )

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
			
				( if ( and ( zerop m ) ( zerop n ) ) ( continue ) )

				( when ( eq ( at state ( + x m ) ( + y n ) ) other-player )
					( format t "Walked from ~D, ~D to ~D, ~D~%" x y ( + x m ) ( + y n ) )
					( setf succ-lst ( append succ-lst ( walk state ( + x m ) ( + y n ) m n ) ) )
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
		)
		( stop return-pos )
	
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
				( setf return-pos ( list ( list x y ) ) )
			)

			; Other player's piece
			( t
				( format t "Other player's piece~%" )
				( setf stop T )
			)

		)
	)
)

;))))))))
#|            
(setf game '(nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil W B nil nil nil
             nil nil nil B W nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             )) 

( defun find-move ( state player )
    ( let
        (
            ( succ-lst nil )
        )
 
        ( ( position player state )
        )
    )       
)

( defun foo ( state pos player )
    ( let
        (
            ( return-lst nil )
            ( other-player ( if ( eq player W ) B W ) )
        )
        
        ; N-W
        ( if ( eq ( nth ( - pos 9 ) state ) other-player )
            ; go time
        )
        
        ; N
        ( if ( eq ( nth ( - pos 8 ) state ) other-player )
            ; go time
        )
        
        ; N-E
        ( if ( eq ( nth ( - pos 7 ) state ) other-player )
            ; go time
        )
        
        ; W
        ( if ( eq ( nth ( - pos 1 ) state ) other-player )
            ; go time
        )
        
        ; E
        ( if ( eq ( nth ( + pos 1 ) state ) other-player )
            ; go time
        )
        
        ; S-W
        ( if ( eq ( nth ( + pos 7 ) state ) other-player )
            ; go time
        )
        
        ; S
        ( if ( eq ( nth ( + pos 8 ) state ) other-player )
            ; go time
        )
        
        ; S-E
        ( if ( eq ( nth ( + pos 9 ) state ) other-player )
            ; go time
        )
    )
)

( defun walk-hoz ( state pos player dir )
    ( let 
        (
            ( row ( nth-value 0 ( floor pos 8 ) ) )
            ( other-player ( if ( eq player W ) B W ) )
        )
    
        ( do ()
            ( ( or
                ( < pos ( * row 8 ) )
                ( >= pos ( * ( 1+ row ) 8 ) )
                ( not ( eq ( nth pos state ) player ) )
            ) nil )
        
            ( setf pos ( + pos dir ) )
        )
        
        ( cond
        
            ; Stepped out of bounds
            ( ( or
                ( < pos ( * row 8 ) )
                ( >= pos ( * ( 1+ row ) 8 ) )
              )
              nil
            )
            
            
            ( ( eq ( nth pos state ) other-player )
              nil
            )
            
            ( t pos )
        )
    )
)

( defun walk-vert ( state pos player dir )
    ( let 
        (
            ( other-player ( if ( eq player W ) B W ) )
        )    

        ( do ()
            ( ( or
                ( < pos 0 )
                ( > pos 63 )
                ( not ( eq ( nth pos state ) player ) )
            ) nil )
            
            ( setf pos ( + pos dir ) )
        )

        ( cond
        
            ; Stepped out of bounds
            ( ( or
                ( < pos 0)
                ( > pos 63 )
              )
              nil
            )
            
            ( ( eq ( nth pos state ) other-player )
              nil
            )
            
            ( t pos )
        )
    )
)
|#




























