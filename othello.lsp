#| 
    othello.lsp
|#

( load 'print )
( load 'heuristics )
( load 'alpha-beta )

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

        ( printBoard ( state-board ( get-start ) ) )
    )
)

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

( defun make-move ( position player ply )
    ( alpha-beta
        ( make-state
            :board position
            :player player
            :moves ( find-move position player )
            :creationMove nil
        )
        ply
        #'gen-successors
        #'evaluate
    )
)

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

            ( setf s ( make-move board player 3 ) )
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
        ( setf s ( make-move board 'W 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "W: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        
        ( setf s ( make-move board 'B 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "B: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        
        ( setf s ( make-move board 'W 3 ) )
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

( defun move-to-state ( curState move ) 
    ( let
        (
            ( path ( cadr move ) )
            ( pos ( car move ) )
            ( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
            newBoard
        )

        ( setf newBoard ( flip-tiles ( state-board curState ) path ) )

        ( place-coin newBoard ( state-player curState ) pos )

;        ( setf
;            ( at
;                newBoard 
;                ( car pos ) 
;                ( cadr pos )
;            )
;            ( state-player curState )
;        )
        
        ( make-state
            :board newBoard
            :player other-player
            :moves ( find-move newBoard other-player )
            :creationMove pos
        )
    )
)

#|
( defun gen-successors ( state player ) 

    ( mapcar #'( lambda ( move ) ( move-to-state state move player ) )
        ( find-move state player )
    )
)
|#

( defun gen-successors ( curState ) 
    ( let
        (
            ;( move-lst ( find-move curState ) )
            ( i -1 )
            max-i
        )
        
        ( setf max-i ( - ( length ( state-moves curState ) ) 1 ) )
        
        ( lambda ()
            ( when ( < i max-i )
                ( incf i )
                ( move-to-state curState ( nth i ( state-moves curState ) ) )
            )
        )
    )
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


( defun flip-tiles ( board path ) 

    ( let (
            player
            ( newBoard ( mapcar #'copy-list board ) )
          )

        ( dolist ( tile path newBoard ) 
            ( setf player ( at newBoard ( car tile ) ( cadr tile ) ) )

            ( setf player ( if ( eq player 'W ) 'B 'W ) )

            ( setf ( at newBoard ( car tile ) ( cadr tile ) ) player )
        )
    )
)

( defun place-coin ( board player pos )
    ( setf
        ( at
            board 
            ( car pos ) 
            ( cadr pos )
        )
        player
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
