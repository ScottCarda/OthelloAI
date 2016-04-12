( defun evaluate ( curState )
    ( +
        ( coin curState )
        ( corners curState )
        ( moves curState )
    )
)

( defun coin ( curState )
	( do
		(
			( i 0 ( 1+ i ) )

			;( player 'B )
			( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )

			;( player-count ( count ( state-player curState ) ( nth i ( state-board curState ) ) )
			;	( + player-count ( count ( state-player curState ) ( nth i ( state-board curState ) ) ) )
			;)
			;( other-player-count ( count other-player ( nth i ( state-board curState ) ) )
			;	( + other-player-count ( count other-player ( nth i ( state-board curState ) ) ) )
			;)
			
			( player-count 0 )
			( other-player-count 0 )
		)
		( ( >= i ( length ( state-board curState ) ) )
			( compare-counts player-count other-player-count )
		)
		
		( setf player-count
		    ( + player-count ( count ( state-player curState ) ( nth i ( state-board curState ) ) ) )
	    )
	    
		( setf other-player-count
		    ( + other-player-count ( count other-player ( nth i ( state-board curState ) ) ) )
	    )
	)	
)

( defun corners ( curState )
	( let
		(
			;( player 'B )
			( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )

			( player-count 0 )
			( other-player-count 0 )
		)
		;( setf other-player ( if ( eq player 'B ) 'W 'B ) )

		( do ( ( i 0 ( + i 7 ) ) ) ( ( >= i 8 ) nil )
		( do ( ( j 0 ( + j 7 ) ) ) ( ( >= j 8 ) nil )
			( cond
				( ( eq ( at ( state-board curState ) i j ) ( state-player curState ) ) ( incf player-count ) )
				( ( eq ( at ( state-board curState ) i j ) other-player ) ( incf other-player-count ) )
			)
		) )

		( if ( zerop ( + player-count other-player-count ) )
			0
			( compare-counts player-count other-player-count )
		)
	)
)

( defun moves ( curState )
    ( let
        (
            ;( player 'B )
            ( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
            
            player-count
            other-player-count
        )
        
        ( setf player-count ( length ( state-moves curState ) ) )
        ( setf other-player-count ( length ( find-move ( state-board curState ) other-player ) ) )
        
        ( if ( zerop ( + player-count other-player-count ) )
            0
            ( compare-counts player-count other-player-count )
        )
        
    )
)

( defun compare-counts ( count1 count2 )
;    ( format t "~tCount1: ~D~%" count1 )
;    ( format t "~tCount2: ~D~%" count2 )

	( * 100 ( /
		( - count1 count2 )
		( + count1 count2 )
	) )
)
