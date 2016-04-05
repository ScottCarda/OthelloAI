#| 
    Print Function for Othello Program

|#


(defun printBoard (game)

    (let ((i 1))
        (format t "  1 2 3 4 5 6 7 8~%")
        (dolist (x game)
            (format t "~a " i)
            (dolist (y x)
                (if (null y)
                    (format t "- ")
                    (format t "~a " y)
                )
            )
            (setf i (1+ i))
            (format t "~%") 
        )
    )
)

; (printBoard game)