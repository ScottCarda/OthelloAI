#|
    **** tournament.lsp ****

Allow two Othello programs to play against one another in a computer tournament.
Runs each program in its own package, to avoid name conflicts.

Usage:  clisp tournament.lsp playerB_folder playerW_folder [depth]
        A file named othello.lsp must be present in each folder.
        This file must contain (or load) the Othello code.
        Required function: (make-move board player depth)

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

|#

; define packages
(defpackage :playerB (:use :cl) (:export :make-move :othello-init))
(defpackage :playerW (:use :cl) (:export :make-move :othello-init))

;=============================== main function =====================================

; emulate a main function
(defun main ( args )
    "(main args): emulate a main function, called with command-line args"

    (let ((depth 4))
        ; handle command-line args (two Othello programs plus optioonal search depth)
        (when (< (length ext:*args*) 2) (format t "Usage: clisp tournament.lsp playerB_folder playerW_folder [depth]~%") (exit))
        (when (= (length ext:*args*) 3) (setf depth (parse-integer (nth 2 ext:*args*))))
        (format t "~%**** ~a (B) vs ~a (W) ****~%" (nth 0 ext:*args*) (nth 1 ext:*args*))
        (format t "~%search depth ~a~%" depth)

        ; load each in separate package
        (cd (nth 0 ext:*args*))
        (in-package :playerB)
        (load 'othello)
        (cd "..")

        (cd (nth 1 ext:*args*))
        (in-package :playerW)
        (load 'othello)
        (cd "..")

        ; restore default CL user package
        (in-package :cl-user)

        ; play one program against another
        (play-othello depth)
    )
)

;==========================  Play Othello =======================================

; play two othello programs against one another
(defun play-othello (depth)
    "(play-othello depth ): play two othello programs against one another, using specified search depth"

    (let
        (
            (board '( - - - - - - - -
                      - - - - - - - -
                      - - - - - - - -
                      - - - W B - - -
                      - - - B W - - -
                      - - - - - - - -
                      - - - - - - - -
                      - - - - - - - - ))

             ; heuristic values
            (heuristics '(1 1 4 2))

            ; track who won and time used
            (timeB 0)
            (timeW 0)
            npieces
        )

        ;; initialization routines
        (playerB:othello-init)
        (playerW:othello-init)

        (format t "~%Initial position:~%")
        (print-board board)

        ; begin play
        (do
            ; loop vars
            ((moveB t) (moveW t) (t1 0) (t2 0))

            ; termination condition: game over
            ((or (game-over board) (and (null moveB) (null moveW)))
                (if (and (null moveB) (null moveW)) (format t "~%Game over, neither player can move~%"))
                (setf npieces (count-pieces board))
            )

            ; move for black
            (setf t1 (get-internal-real-time))
            (setf moveB (playerB:make-move board 'B depth))
            (setf t2 (get-internal-real-time))
            (incf timeB (- t2 t1))
            (cond
                (moveB
                    (setf board (apply-move board (1- (nth 0 moveB)) (1- (nth 1 moveB)) 'B))
                    (format t "~%Player B's move: ~A~%" moveB)
                )
                (t (format t "~%Player B cannot move~%"))
            )
            (print-board board)

            ; move for white (really should check for game over...)
            (setf t1 (get-internal-real-time))
            (setf moveW (playerW:make-move board 'W depth))
            (setf t2 (get-internal-real-time))
            (incf timeW (- t2 t1))
            (cond
                (moveW
                    (setf board (apply-move board (1- (nth 0 moveW)) (1- (nth 1 moveW)) 'W))
                    (format t "~%Player W's move: ~A~%" moveW)
                )
                (t (format t "~%Player W cannot move~%"))
            )
            (print-board board)
        )

        ; output the piece results
        (format t "~%Player B pieces: ~A, time: ~,1F seconds~%" (nth 0 npieces) (/ timeB 10000000.0))
        (format t   "Player W pieces: ~A, time: ~,1F seconds~%" (nth 1 npieces) (/ timeW 10000000.0))
        (values)
    )
)

;==========================  Print Board  =======================================

; Prints the board to the screen
(defun print-board (board)
    "(print-board board): print the board"
   (let ((position board))
        ; print numbers along the top
        (format t " ")
        (dotimes (i 8) (format t " ~A" (+ i 1)))
        (format t "~%")

        ; print each row of the board
        (dotimes (i 8)
             ; print the row number, then the row
            (format t "~A" (+ i 1))
            (dotimes (j 8) (format t " ~A" (nth (+ (* i 8) j) position)))
            (format t "~%")
        )
    )
)

;=========================  Apply Move  ========================================

; If the ith row and jth column is a valid move for the given piece on
; the given board, return a new board with the piece added.
; Board argument is not modified. Return NIL if the move is invalid.

(defun apply-move (board i j piece)
    "apply-move (board i j piece): set piece at row i, col j on board"
    (let
        (
            (position (+ (* i 8) j))
            (new-board (copy-list board))
            (valid nil)
        )

        ; check that the position is empty
        (if (not (equal (nth position board) '-)) (return-from apply-move nil))

        ; place a piece in the position
        (setf (nth position new-board) piece)

        ; try to flip pieces in the various directions
        (dolist (k '(1 -1 8 -8 -9 -7 7 9))
            (if (flip-pieces new-board position piece k 0) (setf valid T))
        )

        ; if flipping pieces was successful in some direction,
        ; then return the new board, otherwise, return nil
        (if valid new-board nil)
    )
)

;========================  Flip Pieces  ======================================

; Recursive function for flipping pieces. Recursive calls continue search
; for another of the given piece. If a match is found, flipping is valid,
; true is returned, and flip the piece at that position.
; board - only modified if flipping direction is valid.
; position - position on board for flipping
; piece - either B or W
; step - indicates flipping direction
; count - auxillary variable used to test for special cases


(defun flip-pieces (board position piece step count)
    "(flip-pieces board position piece step count): flip pieces after each move"
    (let (flip (next-pos (+ position step)))
        (cond
            ; check if we've reached a match to the given piece
            ((and (equal (nth position board) piece) (> count 1)) t)

            ; check if the first match is after one step
            ((and (equal (nth position board) piece) (= count 1)) nil)

            ; check for edge of the board
            ((> 0 next-pos) nil)
            ((< (* 8 8) next-pos) nil)
            ((and (= 7 (mod position 8)) (= 0 (mod next-pos 8))) nil)
            ((and (= 0 (mod position 8)) (= 7 (mod next-pos 8))) nil)

            ; check if we've reached an empty space
            ((equal (nth position board) '-) nil)

            (t
                ; do recursive call
                (setf flip (flip-pieces board next-pos piece step (1+ count)))
                ; flip piece if valid
                (if flip (setf (nth position board) piece))
                ; return flip validity
                flip
            )
        )
    )
)

;==========================  Can Move  ===========================================

; return true if there is a legal move
(defun can-move (board piece)
    "(can-move board piece): T if legal move, NIL if not"
    (dotimes (i 8 nil)
        (dotimes (j 8)
            (if (apply-move board i j piece) (return-from can-move t))
        )
    )
)

;==========================  Game Over  ==========================================

;  return true if the game is over
(defun game-over (board)
    "(game-over board): T if game over, NIL if not"
   (not (or (can-move board 'B) (can-move board 'W)))
)

;==========================  Count Pieces  ======================================

; return number of each piece
(defun count-pieces (board)
    "(count-pieces board): return list with (#B #W) pieces"
    (let ((b 0) (w 0))
        (dolist (pos board (list b w))
            (if (eq pos 'B) (incf b))
            (if (eq pos 'W) (incf w))
        )
    )
)

;==========================  Add Piece to Board  ==============================

; add piece to board (for debugging only)
; move gives board position as 8*r+c, indexed from 0 rather than 1
; note: destructive setf alters board
(defun add-piece ( board move player )
    "(add-piece board move player): add piece to board"
    (let ( (pos (+ (* (nth 0 move) 8) (nth 1 move) -9)) )
        (setf (nth pos board) player)
    )
    board
)

;=========================== call main function =================================

; call the main function, passing command-line arguments
(main ext:*args*)
