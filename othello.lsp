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


(setf game '((nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil "W" "B" nil nil nil) 
             (nil nil nil "B" "W" nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             (nil nil nil nil nil nil nil nil) 
             ))