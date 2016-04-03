( load 'minimax )
( load 'tic-tac-toe )

( defun deepenough ( depth ) ( <= depth 0 ) )

( defun move-generator ( position ) ( successors position ) )

( defun static ( position ) ( eval-state position ) )

( defun auto-play ( n-ply )
	( do
		(
			( result nil )
			( move '( E E E E E E E E E ) )
		)
		( ( not move ) ( values ) )

		( setf result ( minimax move n-ply ) )
		( print-state ( setf move ( car ( cadr result ) ) ) )
	)
)
