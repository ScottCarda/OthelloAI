#|
                    ***** ALPHA-BETA.LSP *****

This file contains the routines for performing the alpha-beta pruning minimax
algorithm. The algorithm is implemented recursively by switching the alpha
and beta values each call and by negating the values of the successor nodes.
This allows the recursive routine to always assume it is the MAX player's turn.

Author: Scott Carda
Written Spring 2016 for CSC447/547 AI class.

|#

#|--------------------------------------------------------------------------|#
#|                    Alpha-Beta Pruning Minimax Algorithm                  |#
#|--------------------------------------------------------------------------|#

; The calling function for the alpha-beta minimax algorithm.
; Returns the chosen successor state.
( defun alpha-beta ( state depth successors eval-state )
"Uses alpha-beta pruning minimax to select and return a successor state."
	; Call the recursive function and return just
	; the state part of the value-state pair
    ( cadr ( minimax
        state
        depth
        nil
        nil
        successors
        eval-state
    ) )
)

; Recursive function for performing the alpha-beta pruning minimax algorithm.
( defun minimax ( state depth alpha beta successors eval-state )
"Recursively evaluates and selects successor nodes using alpha-beta pruning minimax."
    ( let
        (
			; The generator function for successively getting successor states
            ( get-next ( funcall successors state ) )
            succ	; A successor state
			; The best (maximum) evaluation value
			; paired with its corresponding successor state
            best
            succ-val ; The state-value pair returned from a recursive call
        )

		; Generate the first successor if we are not deep enough
        ( if ( > depth 0 )
            ( setf succ ( funcall get-next ) )
        )

	    ( cond 
	    
			; If this is a leaf node
	        ( ( or ( <= depth 0 ) ( not succ ) )
	            ( list ( funcall eval-state state ) )
	        )
	        
	        ( t
	            ; Process first successor
	            ( setf succ-val ( minimax
					; Use the successor as the state for the recursive call
					succ
					; One ply deeper
					( 1- depth )
					; Swap alpha and beta to reflect the chage in player
					beta alpha
					; Pass in the successor and eval-state functions
					successors eval-state
				) )
	            ; Change sign to reflect the change in player
                ( setf ( car succ-val ) ( - ( car succ-val ) ) )
	            ; Set best as first successor
	            ( setf best ( list ( car succ-val ) succ ) )
	            
				; Loop through the rest of the successors,
				; or until prune condition is met
	            ( do ()
	                (
	                    ( or
							; Prune condition
	                        ( and alpha ( <= ( - ( car best ) ) alpha ) )
							; End of successors
	                        ( not ( setf succ ( funcall get-next ) ) )
                        )
                        best	; Return the best value-state pair
                    )
	                
					; Recursive call for minimax
	                ( if
                        ( or
                            ( not beta )
                            ( < beta ( car best ) )
	                    )
	                    ; If best is better than beta, use best instead of beta, and switch alpha and beta
                        ( setf succ-val ( minimax succ ( 1- depth ) ( car best ) alpha successors eval-state ) )
						; Otherwise, just switch alpha and beta, to reflect change in player
	                    ( setf succ-val ( minimax succ ( 1- depth ) beta alpha successors eval-state ) )
                    )
                    
                    ; Change sign to reflect the change in player
                    ( setf ( car succ-val ) ( - ( car succ-val ) ) )

                    ; Updates the values of best, if need be
                    ( if ( > ( car succ-val ) ( car best ) )
                        ( setf best ( list ( car succ-val ) succ ) )
                    )
	            )
            )
        )
    )
)
