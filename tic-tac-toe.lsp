( defun row-and-col ( state pos )
	( do*
		(
			( i 0 ( 1+ i ) )

			( N ( isqrt ( length state ) ) )

			( row-n ( * ( nth-value 0 ( floor pos N ) ) N ) ( 1+ row-n ) )
			( col-n ( nth-value 1 ( floor pos N ) ) ( + col-n N ) )

			( row nil )
			( col nil )
		)
		( ( >= i N ) ( list row col ) )

		( setf row ( nconc row ( list ( nth row-n state ) ) ) )
		( setf col ( nconc col ( list ( nth col-n state ) ) ) )
	)
)

( defun sample () '( X E O X X E O O X ) )

( defun diagonals ( state )
	( do*
		(
			( i 0 ( 1+ i ) )
			
			( N ( isqrt ( length state ) ) )

			( back-n 0 ( + back-n N 1 ) )
			( forward-n ( - N 1 ) ( + forward-n ( - N 1 ) ) )

			( back nil )
			( forward nil )
		)
		( ( >= i N ) ( list back forward ) )

		( setf back ( nconc back ( list ( nth back-n state ) ) ) )
		( setf forward ( nconc forward ( list ( nth forward-n state ) ) ) )
	)
)

( defun get-all ( state )
	( do*
		(
			( i 0 ( 1+ i ) )

			( N ( isqrt ( length state ) ) )
			( pos 0 ( + pos N 1 ) ) 

			( all ( diagonals state ) )
		)
		( ( >= i N ) all )

		( setf all ( nconc all ( row-and-col state pos ) ) )
	)
)

( defun eval-state ( state )
	( let ( ( val 0 ) )
		( dolist ( i ( get-all state ) val )
			( unless ( member 'O i )
				( setf val ( + val ( expt 10 ( count 'X i ) ) ) )
			)
			( unless ( member 'X i )
				( setf val ( - val ( expt 10 ( count 'O i ) ) ) )
			)
		)
	)
)

( defun testgen ( state )
	( let
		(
			( i 0 )
			mark
		)
	
		( if ( eq ( count 'X state ) ( count 'O state ) )
			( setf mark 'X )
			( setf mark 'O )
		)

		( lambda ()
			( let ( succ )
				( cond

					( ( not i ) nil )

					( ( setf i ( position 'E state :start i ) )
						( setf succ ( copy-list state ) )
						( setf ( nth i succ ) mark )
						( incf i )
						succ
					)

					( t nil )
				)
			)
		)
	)	
)

( defun successors ( state )
	( let ( ( succ-list nil ) succ mark )
		( if ( eq ( count 'X state ) ( count 'O state ) )
			( setf mark 'X )
			( setf mark 'O )
		)
		( dotimes ( i ( length state ) succ-list )
			( when ( eq ( nth i state ) 'E )
				( setf succ ( copy-list state ) )
				( setf ( nth i succ ) mark )
				( setf succ-list ( cons succ succ-list ) )
			)
		)
	)
)

( defun print-state ( state )
	( when state 
		( format t "+---+---+---+~%" )
		( do
			(
				( i 0 ( 1+ i ) )
			
				( N ( isqrt ( length state ) ) )

				( row nil )
			)
			( ( >= i N ) ( values ) )

			( setf row ( subseq state ( * N i  ) ( * N ( 1+ i ) ) ) )

			( format t "|   |   |   |~%" )
			( format t "| ~{~A ~}~%"
				( apply #'append ( mapcar #'( lambda ( elem ) ( list ( if ( eq elem 'E ) " " elem ) "|" ) ) row ) )
			)
			( format t "|   |   |   |~%" )
			( format t "+---+---+---+~%" )	
		)
	)
)

