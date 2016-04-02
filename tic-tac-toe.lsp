( defun row-and-col ( state pos )
;	( let
;		(
;			( N ( isqrt ( length state ) ) )
;			row-n ; Start position of row
;			col-n ; Start position of col
;		)
;
;		( multiple-value-bind ( div remain ) ( floor pos N )
;			( setf row-n ( * div N  ) )
;			( setf col-n remain )
;		)

		;( format t "N: ~D~%row-n: ~D~%col-n: ~D~%" N row-n col-n )

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
			;( setf row-n ( 1+ row-n ) )

			( setf col ( nconc col ( list ( nth col-n state ) ) ) )
			;( setf col-n ( + col-n N ) )
		)
;	)
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
