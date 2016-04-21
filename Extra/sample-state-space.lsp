( defun gen-successors ( state )
	( list 
		( append state ( list 1 ) )
		( append state ( list 2 ) )
		( append state ( list 3 ) )
	)
)

( defun successors ( state )
	( let ( ( i 0 ) )
	    ( lambda ()
            ( when ( < i 3 )
                ( incf i )
                ( append state ( list i ) )
            )
	    )
	)
)

( let ( ( vals '(
;-6 -16 5 6 -17 #|20|# 12 -13 14    -10 -17 0 2 -13 -15 11 -9 -16    19 17 -4 #|2 -13 -2 -11 -8 12|#
;14 7 -18 -13 3 -8 -11 -2 17    -12 -3 5 -13 #|-7 8|# 20 6 -4    -2 19 13 #|2 -19 9 -9 11 -10|#
;19 -20 #|8|# 9 17 17 10 -7 #|10|#    -9 20 15 19 12 19 #|-14 -3 6|#    14 11 -14 -9 4 -18 -3 -15 #|16|#

;-6 -16 5 6 -17 20 12 -13 14    -10 -17 0 2 -13 -15 11 -9 -16    19 17 -4 2 -13 -2 -11 -8 12
;14 7 -18 -13 3 -8 -11 -2 17    -12 -3 5 -13 -7 8 20 6 -4    -2 19 13 2 -19 9 -9 11 -10
;19 -20 8 9 17 17 10 -7 10    -9 20 15 19 12 19 -14 -3 6    14 11 -14 -9 4 -18 -3 -15 16

;-5 -2 7    -2 5 18    1 12 #|-6|#
;-2 -5 -13    #|4 -14 -8    -18 -15 -6|#
;-14 5 18    4 -11 -20    #|-3 13 -1|#

-5 -2 7    -2 5 18    1 12 -6
-2 -5 -13    4 -14 -8    -18 -15 -6
-14 5 18    4 -11 -20    -3 13 -1

) ) ( i -1 ) )

( defun eval-state ( state )
	( when ( < i ( length vals ) )
		( incf i )
		( if ( evenp ( length state ) )
			( nth i vals )
			( - ( nth i vals ) )
		)
	)
)

( defun reset-eval ()
	( setf i -1 )
	( values )
)

)

( load 'alpha-beta )

( defun run-minimax ( state depth )
    
    ( minimax
        state
        depth
        nil
        nil
        #'successors
        #'eval-state
    )

)
