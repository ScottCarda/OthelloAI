( defun gen-successors ( curState ) 
    ( let
        (
            ;( move-lst ( find-move curState ) )
            ( i -1 )
            max-i
        )
        
        ( setf max-i ( - ( length ( state-moves curState ) ) 1 ) )
        
        ( lambda ()
            ( when ( < i max-i )
                ( incf i )
                ( move-to-state curState ( nth i ( state-moves curState ) ) )
            )
        )
    )
)


( defun move-to-state ( curState move ) 
    ( let
        (
            ( path ( cadr move ) )
            ( pos ( car move ) )
            ( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
            newBoard
        )

        ( setf newBoard ( flip-tiles ( state-board curState ) path ) )

        ( if pos
            ( place-coin newBoard ( state-player curState ) pos )
        )

;        ( setf
;            ( at
;                newBoard 
;                ( car pos ) 
;                ( cadr pos )
;            )
;            ( state-player curState )
;        )
        
        ( make-state
            :board newBoard
            :player other-player
            :moves ( find-move newBoard other-player )
            :creationMove pos
        )
    )
)

( defun flip-tiles ( board path ) 

    ( let (
            player
            ( newBoard ( mapcar #'copy-list board ) )
          )

        ( dolist ( tile path newBoard ) 
            ( setf player ( at newBoard ( car tile ) ( cadr tile ) ) )

            ( setf player ( if ( eq player 'W ) 'B 'W ) )

            ( setf ( at newBoard ( car tile ) ( cadr tile ) ) player )
        )
    )
)

( defun place-coin ( board player pos )
    ( setf
        ( at
            board 
            ( car pos ) 
            ( cadr pos )
        )
        player
    )
)

#|
( defun gen-successors ( state player ) 

    ( mapcar #'( lambda ( move ) ( move-to-state state move player ) )
        ( find-move state player )
    )
)
|#
