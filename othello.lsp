#| 
    othello.lsp
|#

(load 'print.lsp)



(defun othello ()

    (let ((input) (move))
        (princ "Would you like to move first [y/n]? ")
        (setf input (read))

        (cond

            ((string= input "Y")
                (format t "OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%")
            )
            ((string= input "N")
                (format t "OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%")
            )

            (t
                (format t "Please enter either y or n.~%")
                (othello)
            )
        )
        (printBoard game)
    )
)


#|(setf game '((nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil W B nil nil nil) 
             (nil nil nil B W nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil)
             ))
|#

             
(setf game '(nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil W B nil nil nil
             nil nil nil B W nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             nil nil nil nil nil nil nil nil
             ))
             
( defun find-move ( state player )
    ( let
        (
            ( succ-lst nil )
        )
 
        ( ( position player state )
        )
    )       
)

( defun foo ( state pos player )
    ( let
        (
            ( return-lst nil )
            ( other-player ( if ( eq player W ) B W ) )
        )
        
        ; N-W
        ( if ( eq ( nth ( - pos 9 ) state ) other-player )
            ; go time
        )
        
        ; N
        ( if ( eq ( nth ( - pos 8 ) state ) other-player )
            ; go time
        )
        
        ; N-E
        ( if ( eq ( nth ( - pos 7 ) state ) other-player )
            ; go time
        )
        
        ; W
        ( if ( eq ( nth ( - pos 1 ) state ) other-player )
            ; go time
        )
        
        ; E
        ( if ( eq ( nth ( + pos 1 ) state ) other-player )
            ; go time
        )
        
        ; S-W
        ( if ( eq ( nth ( + pos 7 ) state ) other-player )
            ; go time
        )
        
        ; S
        ( if ( eq ( nth ( + pos 8 ) state ) other-player )
            ; go time
        )
        
        ; S-E
        ( if ( eq ( nth ( + pos 9 ) state ) other-player )
            ; go time
        )
    )
)

( defun walk-hoz ( state pos player dir )
    ( let 
        (
            ( row ( nth-value 0 ( floor pos 8 ) ) )
            ( other-player ( if ( eq player W ) B W ) )
        )
    
        ( do ()
            ( ( or
                ( < pos ( * row 8 ) )
                ( >= pos ( * ( 1+ row ) 8 ) )
                ( not ( eq ( nth pos state ) player ) )
            ) nil )
        
            ( setf pos ( + pos dir ) )
        )
        
        ( cond
        
            ; Stepped out of bounds
            ( ( or
                ( < pos ( * row 8 ) )
                ( >= pos ( * ( 1+ row ) 8 ) )
              )
              nil
            )
            
            
            ( ( eq ( nth pos state ) other-player )
              nil
            )
            
            ( t pos )
        )
    )
)

( defun walk-vert ( state pos player dir )
    ( let 
        (
            ( other-player ( if ( eq player W ) B W ) )
        )    

        ( do ()
            ( ( or
                ( < pos 0 )
                ( > pos 63 )
                ( not ( eq ( nth pos state ) player ) )
            ) nil )
            
            ( setf pos ( + pos dir ) )
        )

        ( cond
        
            ; Stepped out of bounds
            ( ( or
                ( < pos 0)
                ( > pos 63 )
              )
              nil
            )
            
            ( ( eq ( nth pos state ) other-player )
              nil
            )
            
            ( t pos )
        )
    )
)





























