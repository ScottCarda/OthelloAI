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
		( nil nil nil nil nil nil nil nil )
	        ( nil nil nil nil  W  nil nil nil )
	        ( nil nil nil  B   W   B  nil nil )
	        ( nil nil nil  B   B  nil nil nil )
	        ( nil nil nil  B   B   B  nil nil )
	        ( nil nil nil nil nil nil nil nil )
	        ( nil nil nil nil nil nil nil nil )
	        ( nil nil nil nil nil nil nil nil )
	)
)


( defmacro at ( state X Y )
	`( nth ,X ( nth ,Y ,state ) )
)

( defun find-move ( state player )
	( do
		(
			( i 0 ( 1+ i ) )
			j

			( succ-lst nil )
			( other-player ( if ( eq player 'W ) 'B 'W ) )
		)
		( ( >= i 8 ) ( remove-duplicates succ-lst :test #'equal ) )

		( setf j ( position player ( nth i state ) ) )

		; Piece found @ state[i][j]
		( when j

			( format t "Found piece at ~D, ~D~%" i j )

			; For each neighbor of state[i][j]
			( do ( ( m -1 ( 1+ m ) ) ) ( ( >= m 2 ) nil )
			( do ( ( n -1 ( 1+ n ) ) ) ( ( >= n 2 ) nil )
			( if ( and ( zerop m ) ( zerop n ) ) ( continue ) )

			( when ( eq ( at state ( + i m ) ( + j n ) ) other-player )
				( format t "Walked from ~D, ~D to ~D, ~D~%" i j ( + i m ) ( + j n ) )
				( setf succ-lst ( append succ-lst ( walk state ( + i m ) ( + j n ) m n ) ) )
				;( format t "There~%" )
			)

			) )
				
		)
	)
)

( defun walk ( state i j m n )
	( do
		(
			( player ( at state i j ) )
			( stop nil )
			( return-pos nil )
		)
		( stop return-pos )
	
		( format t "Walked from ~D, ~D to " i j )

		; Move current position acording to the direction
		( setf i ( + i m ) )
		( setf j ( + j n ) )

		( format t "~D, ~D~%" i j )

		( cond

			; Out of bounds
			( ( or ( < i 0 ) ( >= i 8 ) ( < j 0 ) ( >= j 8 ) )
				( format t "Out of Bounds~%" )
				( setf stop T )
			)

			; Another of the player's pieces
			( ( eq ( at state i j ) player ) )

			; Empty spot
			( ( not ( at state i j ) )
				( format t "Found move at ~D, ~D~%" i j )
				( setf stop T )
				( setf return-pos ( list ( list i j ) ) )
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
    ( do ()
        ( ( or
            ( < pos 0 )
            ( > pos 63 )
            ( not ( eq ( nth pos state ) player ) )
        ) nil )
        
        ( setf pos ( + pos dir ) )
    )
)
|#




























