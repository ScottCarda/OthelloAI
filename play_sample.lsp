( load 'weiss-minimax )
( load 'sample-state-space )

( defun deepenough ( depth ) ( <= depth 0 ) )

( defun move-generator ( position ) ( gen-successors position ) )

( defun static ( position ) ( eval-state position ) )
