#|
                       ***** GENERATE.LSP *****

    This file contains the gen-successors method, as well as other methods
that are used by the gen-successors method. The gen-successors function is
how we are navigating the search space for the Othello board game. It will
take a state, which will have a list of possible moves, and return a function
that, when called, will process the next move on the list and return the
state that the move will create. Note that this function is unique to the
state given to the gen-successors function.

Authors: Allison Bodvig, Scott Carda
Written Spring 2016 for CSC447/547 AI class.

|#

#|--------------------------------------------------------------------------|#
#|                             Gen-Successors                               |#
#|--------------------------------------------------------------------------|#

; Creates and returns a lambda that acts as a successor-generating
; function specifically for the given state.
( defun gen-successors ( curState )
"Return a successor-generating function specific to given state."
    ( let
        (
            ( i -1 ) ; The index of the lambda's current move
            max-i ; The maximum number moves in the move list
        )

        ; The maximum number moves in the move list
        ( setf max-i ( - ( length ( state-moves curState ) ) 1 ) )

        ; The successor-generating function specific to the given state
        ( lambda ()
            ; Return nil if all the successors have been generated
            ( when ( < i max-i )
                ( incf i ) ; Use next move on list
                ; Generate the new state the move makes
                ( move-to-state curState ( nth i ( state-moves curState ) ) )
            )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                             Helper Methods                               |#
#|--------------------------------------------------------------------------|#

; Takes a list of board positions and changes the ownership of the coins at those positions.
( defun flip-tiles ( board path )
"Toggles the coin colors at the given list of positions."
    ( let
        (
            player ; The color marker for the current player
            ( newBoard ( mapcar #'copy-list board ) ) ; A local copy of the game board
        )

        ; For each position in the path
        ( dolist ( tile path newBoard )
            ; Get the player's color at this position
            ( setf player ( at newBoard ( car tile ) ( cadr tile ) ) )

            ; Toggle the player's color
            ( setf player ( if ( eq player 'W ) 'B 'W ) )

            ; Set the new player's color at this position
            ( setf ( at newBoard ( car tile ) ( cadr tile ) ) player )
        )
    )
)

; Takes a move, which is a placing position followed by a list of affected positions,
; and creates a new game state with the given move executed.
( defun move-to-state ( curState move )
"Returns a game state that is updated to have the given move executed."
    ( let
        (
            ( path ( cadr move ) ) ; The list of coin positions that will be changed by this move
            ( pos ( car move ) ) ; The position where the player would place their coin
            ; The color marker of the player who is not the current player
            ( other-player ( if ( eq ( state-player curState ) 'B ) 'W 'B ) )
            ; The board state after the move has been made
            newBoard
        )

        ; Use the path of the move to toggle the ownership of all the appropriate coins
        ( setf newBoard ( flip-tiles ( state-board curState ) path ) )

        ; Place the coin that is indicative of the move
        ( if pos
            ( place-coin newBoard ( state-player curState ) pos )
        )

        ; Generate a new game state based on the new board state
        ( make-state
            :board newBoard
            :player other-player
            :moves ( find-move newBoard other-player )
            :creationMove pos
        )
    )
)

; Places a coin for the given player at the given position
( defun place-coin ( board player pos )
"Places a coin for the given player at the given position."
    ( setf
        ( at
            board
            ( car pos )
            ( cadr pos )
        )
        player
    )
)
