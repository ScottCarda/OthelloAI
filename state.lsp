#|
                       ***** STATE.LSP *****

This file contains the structure definition of a state for
the Othello program, as well as some routines that are closely
tied to the definition of the state.

Authors: Allison Bodvig, Scott Carda
Written Spring 2016 for CSC447/547 AI class.

|#

#|--------------------------------------------------------------------------|#
#|                             State Structure                              |#
#|--------------------------------------------------------------------------|#

( defstruct state
    board   ; The 8 x 8 board state ( list of lists )
    player  ; Whose turn it is ( B | W )
    moves   ; The possible moves for the player whose turn it is
    creationMove ; The position ( x y ) of the move that created the state
)

#|--------------------------------------------------------------------------|#
#|                           Start State Routine                            |#
#|--------------------------------------------------------------------------|#

; Returns the start state of the Othello board game.
( defun get-start ()
"Returns the start state of the Othello board game."
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
            ( player 'B ) ; Black goes first in Othello
        )
	    ( make-state
            :board board
            :player player
            :moves ( find-move board player )
            :creationMove nil ; The start state was not created with a move
        )
    )
)
 
( defun get-sample ()
    ( let
        (
            ( board
                #|'(
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil  B  nil nil nil )
                ( nil nil nil nil  B  nil nil nil )
                ( nil  W   W  nil  B  nil nil nil )
                ( nil  W   W  nil  B   B  nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                )|#
            
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
                #|'(
                ( nil nil nil nil nil nil  W  nil )
                ( nil nil nil nil  B   W  nil nil )
                ( nil nil nil nil  B  nil nil nil )
                ( nil  W   W   B   B  nil nil nil )
                ( nil nil  B   B   B   B  nil nil )
                ( nil nil nil  W  nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ( nil nil nil nil nil nil nil nil )
                ) |#
            )
            ( player 'B )
        )
	    ( make-state 
            :board board
            :player player
            :moves ( find-move board player )
            :creationMove '( 4 1 )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                                 At Macro                                 |#
#|--------------------------------------------------------------------------|#

; Macro for accessing ( getting and setting )
; positions on board using x y coordinates.
( defmacro at ( board X Y )
"Macro for accessing ( getting and setting ) positions on board using x y coordinates."
    `( nth ,X ( nth ,Y ,board ) )
)

#|--------------------------------------------------------------------------|#
#|                           Move-Finding Routines                          |#
#|--------------------------------------------------------------------------|#

; Returns a list of all legal moves for the given player on the given board.
( defun find-move ( board player )
"Finds all legal moves for the given player on the given board."
    ; Looping througth the rows of the board
	( do
		(
		    ; The y coordinate of the positions on the board
			( y 0 ( 1+ y ) )

			( move-lst nil ) ; List of legal moves
			; The color ( W | B ) of the player that was not passed in
			( other-player ( if ( eq player 'B ) 'W 'B ) )
			; A found move ( placing position paired with positions of affected coins )
			move
		)
		( ( >= y 8 )
		    ; Once the whole board has been searched for moves,
		    ; merge the moves such that no two moves have the same
		    ; placing position and each placing position is paired
		    ; with the positions of all affected coins
		    ( merge-path move-lst )
	    )
        ; Looping through the columns of the board
		( do
			(
			    ; The x coordinate of the positions on the board
			    ; Only set to successive positions of coins of the given player 
				( x
				    ; Find the first of player's coins
					( position player ( nth y board ) )
					; Find the successive coins
					( position player ( nth y board ) :start ( 1+ x ) )
				)
			)
			( ( not x ) nil ) ; Stop when no more of player's coins are found

			; For each neighbor of board[x][y]
			( do ( ( m -1 ( 1+ m ) ) ) ( ( >= m 2 ) nil )
			( do ( ( n -1 ( 1+ n ) ) ) ( ( >= n 2 ) nil )
			
			    ( when
			        ( and
			            ; Skip out-of-bounds positions
			            ( >= ( + x m ) 0 ) ( >= ( + y n ) 0 )
			            ; When a neighbor is the other player's piece
			            ( eq ( at board ( + x m ) ( + y n ) ) other-player )
		            )

                    ; Determine if that direction will provide a legal move
                    ( setf move ( walk board ( + x m ) ( + y n ) m n ) )

                    ; If a legal move is found, add it to the move list
                    ( if move 
                        ( setf move-lst ( cons move move-lst ) )
                    )
			    )
			    
			) )
		)
	)
)

; Takes a list of moves with possible non-unique placing positions
; ( the position where the coin is places ) and returns a list of
; moves with unique placing positions by combining the lists of
; affected tiles for each move ( the move's path ).
( defun merge-path ( moves )
"Combines moves with the same placing positions."
    ( let
        (
            ( return-lst nil )  ; The list of moves whose paths have been merged
            ( move-lst ( copy-list moves ) ) ; Local copy of move list
            in-lst  ; Matching move found on return-lst
        )
        
        ; For each move in move-lst
        ; Returns return-lst
        ( dolist ( move move-lst return-lst )
        
            ; Checks if move is already in the return list
            ( setf in-lst ( car ( member move return-lst :test 
                #'( lambda ( m1 m2 ) ( equal ( car m1 ) ( car m2 ) ) 
                  ) 
            ) ) )

            ; If the move is already in the return list
            ( if in-lst
            
                ; Merges the path of the move found in the
                ; return list with the path of the current move
                ( setf ( nth 1 in-lst ) ( append ( nth 1 move ) ( nth 1 in-lst ) ) )
                
                ; Else, add the move to the return list
                ( setf return-lst ( cons move return-lst ) )
            )
        )
    )
)

; Takes a board, an x y coordinates, and m n coordinate update
; values ( simulates direction ) and updates the coordinates with
; m and n until it finds a change in the player's coins at the
; position. Will return a position of the change and the path
; taken to the position if the change in player's coins was the
; result of finding an empty position.
( defun walk ( board x y m n )
"Returns the the end of a line of filled with the one player's coins.
Only returns non-nil if the end is a result of an empty position."
	( do
		(
		    ; The color of the coins to walk along
			( player ( at board x y ) )
			( stop nil ) ; Boolean for when to stop the loop
			( move nil ) ; The move to return
            ( path nil) ; The path taken to the placing position
		)
		( stop move )

        ; Add the current position to the path
        ( setf path ( cons ( list x y )  path ) )

		; Move current position acording to the direction
		( setf x ( + x m ) )
		( setf y ( + y n ) )

		( cond

			; Out of bounds
			( ( or ( < x 0 ) ( >= x 8 ) ( < y 0 ) ( >= y 8 ) )
			    ; Stop without setting move
				( setf stop T )
			)

			; Another of the player's pieces
			( ( eq ( at board x y ) player )
			    ; Just continue the loop
			    ; This is here to prevent later conditions
			)

			; Empty spot
			( ( not ( at board x y ) )
			    ; Stop and set current position as an legal move
				( setf stop T )
				( setf move ( list ( list x y ) path ) )
			)

			; Other player's piece
			( t
			    ; Stop without setting move
				( setf stop T )
			)

		)
	)
)
