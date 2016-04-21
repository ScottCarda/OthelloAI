; Game state evaluation function that combines the tree other evaluation functions.
( defun evaluate ( curState )
"Game state evaluation function that combines three other evaluation functions."
    ( +
        ( coin curState ) ; Number of coins
        ( corners curState ) ; Number of corners taken
        ( moves curState ) ; Number of moves available
    )
)

; Game state evaluation function based on the number of coins each player has on the board.
; Returns positive values for the states that favor the current player.
( defun coin ( curState )
"Game state evaluation function based on the number of coins on the board."
	; For each row on the board
	( do
		(
			; The loop variable
			( i 0 ( 1+ i ) )

			; The color marker for the other player
			( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
			
			( player-count 0 ) ; The statistic for the current player
			( other-player-count 0 ) ; The statistic for the other player
		)
		; Stop when you have processed all rows
		( ( >= i ( length ( state-board curState ) ) )
			; Return the comparison value of the statistics
			( compare-counts player-count other-player-count )
		)
		
		; Counts the number of coins the current player has on the board
		( setf player-count
		    ( + player-count ( count ( state-player curState ) ( nth i ( state-board curState ) ) ) )
	    	)
	    
		; Counts the number of coins the other player has on the board
		( setf other-player-count
		    ( + other-player-count ( count other-player ( nth i ( state-board curState ) ) ) )
	    	)
	)	
)

; Game state evaluation function based on the number of corners both players have taken.
; Returns positive values for the states that favor the current player.
( defun corners ( curState )
"Game state evaluation function based on the number of corners taken."
	( let
		(
			; The color marker for the other player
			( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )

			( player-count 0 ) ; The statistic for the current player
			( other-player-count 0 ) ; The statistic for the other player
		)

		; Check each of the corners
		( do ( ( i 0 ( + i 7 ) ) ) ( ( >= i 8 ) nil )
		( do ( ( j 0 ( + j 7 ) ) ) ( ( >= j 8 ) nil )
			( cond
				; If the corner is occupied by the current player' coin
				( ( eq ( at ( state-board curState ) i j ) ( state-player curState ) )
					( incf player-count )
				)
				; If the corner is occupied by the other player's coin
				( ( eq ( at ( state-board curState ) i j ) other-player )
					( incf other-player-count )
				)
			)
		) )

		( if ( zerop ( + player-count other-player-count ) )
			0 ; If the statistics are both zero, return 0
			; Else return the comparison value of the statistics
			( compare-counts player-count other-player-count )
		)
	)
)

; Game state evaluation function based on the number of moves both players can make.
; Returns positive values for states that favor the current player.
( defun moves ( curState )
"Game state evlauation function based on number of possible moves."
    ( let
        (
	    ; The color marker for the other player
            ( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
            
            player-count ; The statistic for the current player
            other-player-count ; The statistic for the other player
        )
        
	; Counts the number of moves the current player can make
        ( setf player-count ( length ( state-moves curState ) ) )
	; Counts the number of moves the other player can make
        ( setf other-player-count ( length ( find-move ( state-board curState ) other-player ) ) )
        
        ( if ( zerop ( + player-count other-player-count ) )
            0 ; If the statistics are both zero, return 0
	    ; Else return the comparison value of the statistics
            ( compare-counts player-count other-player-count )
        )
        
    )
)

; Comparison function used in game state evaluation functions.
( defun compare-counts ( count1 count2 )
"Comparison function used in game state evaluation."
	( * 100 ( /
		( - count1 count2 )
		( + count1 count2 )
	) )
)
