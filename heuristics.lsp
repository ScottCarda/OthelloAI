( defun coin ( state )
	( do*
		(
			( i 0 ( 1+ i ) )

			( player 'B )
			( other-player ( if ( eq player 'B ) 'W 'B ) )

			( player-count ( count player ( nth i state ) )
				( + player-count ( count player ( nth i state ) ) )
			)
			( other-player-count ( count other-player ( nth i state ) )
				( + other-player-count ( count other-player ( nth i state ) ) )
			)
		)
		( >= i ( length state )
			( compare-counts player-count other-player-count )
		)
	)	
)

#|( defun coin ( state )
	( let
		(
			( player 'B )
			( player-count 0 )
			other-player
			( other-player-count 0 )
		)
		( setf other-player ( if ( eq player 'B ) 'W 'B ) )

		( do*
			(
				( i 0 ( 1+ i ) )

				( player 'B )
				( other-player ( if ( eq player 'B ) 'W 'B ) )

				( player-count ( count player ( nth i state ) )
					( + player-count ( count player ( nth i state ) ) )
				)
				( other-player-count ( count other-player ( nth i state ) )
					( + other-player-count ( count other-player ( nth i state ) ) )
				)
			)
			( >= i ( length state )
				( /
					( * 100 ( - player-count other-player-count ) )
					( + player-count other-player-count )
				)
			)

			;( setf player-count ( + player-count ( count player ( nth i state ) ) ) )
			;( setf other-player-count ( + other-player-count ( count other-player ( nth i state ) ) ) )
		)

	)
)|#

( defun corners ( state )
	( let
		(
			( player 'B )
			other-player

			( player-count 0 )
			( other-player-count 0 )
		)
		( setf other-player ( if ( eq player 'B ) 'W 'B ) )

		( do ( ( i 0 ( + i 7 ) ) ) ( ( >= i 8 ) nil )
		( do ( ( j 0 ( + j 7 ) ) ) ( ( >= j 8 ) nil )
			( cond
				( ( eq ( at state i j ) player ) ( incf player-count ) )
				( ( eq ( at state i j ) other-player ) ( incf other-player-count ) )
			)
		) )

		( if ( zerop ( + player-count other-player-count ) )
			0
			( compare-counts player-count other-player-count )
		)
	)
)

( defun compare-counts ( count1 count2 )
	( * 100 ( /
		( - count1 count2 )
		( + count1 count2 )
	) )
)
