( defun test2 ()
    ( let
        (
            s
            move
            ( board ( state-board ( get-start ) ) )
            ( player 'B )
            ( input 'y )
        )
        
        ( printboard board )
        
        ( do ()
            ( nil ( values ) )

            ( setf s ( make-move-state board player 3 ) )
            ( cond
            
                ( s
                    ( setf move ( state-creationMove s ) )
                    ( setf board ( state-board s ) )
                    ( format t "~%~A: ~A~%" player ( mapcar #'1+ move ) )
                    ( printboard board )
                    
                )
                
                ( t
                    ( format t "Pass~%" )
                )
            )
            
            ( setf player ( if ( eq player 'B ) 'W 'B ) )
            
            ( setf input ( read ) )
            
        )
        
        #|
        ( setf s ( make-move-state board 'W 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "W: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        
        ( setf s ( make-move-state board 'B 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "B: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        
        ( setf s ( make-move-state board 'W 3 ) )
        ( setf move ( state-creationMove s ) )
        ( setf board ( state-board s ) )
        ( format t "W: ~A~%" ( mapcar #'1+ move ) )
        ( printboard board )
        |#
    )
)

( defun test ( curState )
    ( let
        (
            ( successors ( gen-successors curState ) )
            ( succ nil )
        )
        
        ( setf succ ( funcall successors ) )
        
        ( do ()
            ( ( not succ ) nil )
            
            ( printBoard ( state-board succ ) )
            
            ( setf succ ( funcall successors ) )
            
        )
        
        ;( mapcar #'printBoard ( gen-successors state 'B ) )
    )
    ( values )
)
