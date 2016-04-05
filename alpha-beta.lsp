( load sample-state-search )

( defun minimax ( state depth alpha beta )
    ( let
        (
            ( get-next ( successors state ) )
            succ
            best
            succ-val
        )

        ( setf succ ( funcall get-next ) )

	    ( cond 
	    
	        ( ( or ( <= depth 0 ) ( not succ ) )
	            ( let ( ( val ( eval-state state ) ) )
                ( format t "~A:~%" state )
	            ( format t "    ~D~%" val )
	            ( format t "    Leaf~%~%" )
	            ( list val )
	            )
	            ;( list ( eval-state state ) nil )
	        )
	        
	        ( t
	            ; Process first successor
	            ( setf succ-val ( minimax succ ( 1- depth ) beta alpha ) )
	            ; Change sign to reflect the change in player
                ( setf ( car succ-val ) ( - ( car succ-val ) ) )
	            ; Set best as first successor
	            ( setf best ( list ( car succ-val ) succ ) )

                ( format t "~A:~%" state )
	            ( format t "    ~D~%" ( car best ) )
	            ( format t "    ~A~%~%" ( cadr best ) )
	            
	            ( do ()
	                (
	                    ( or
	                        ( and alpha ( <= ( - ( car best ) ) alpha ) ) ; Prune condition
	                        ( not ( setf succ ( funcall get-next ) ) ) ; End of successors
                        )
                        
                        ( if ( and alpha ( <= ( - ( car best ) ) alpha ) ) ; Prune condition
                            ( format t "Pruned!~%~%" )
                        )
                        
                        ( format t "Done with children of ~A.~%~%" state )
                        
                        best
                    )
	                
	                ( if
                        ( or
                            ( not beta )
                            ( < beta ( car best ) )
	                    )
	                    
                        ( setf succ-val ( minimax succ ( 1- depth ) ( car best ) alpha ) )
	                    ( setf succ-val ( minimax succ ( 1- depth ) beta alpha ) )
                    )
                    
                    ; Change sign to reflect the change in player
                    ( setf ( car succ-val ) ( - ( car succ-val ) ) )

                    ; Updates the values of best, if need be
                    ( if ( > ( car succ-val ) ( car best ) )
                        ( setf best ( list ( car succ-val ) succ ) )
                    )
                    
                    ( format t "~A:~%" state )
	                ( format t "    ~D~%" ( car best ) )
	                ( format t "    ~A~%~%" ( cadr best ) )
                
	            )
            )
        )
    )
)
