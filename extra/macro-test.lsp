( defmacro mac1 ()
    `( format t "Macro1~%" )
    ;"Macro1~%"
)

( defun test1 ()
    ( format t "Test1 before macro.~%" )
    ( mac1 )
    ;( format t ( mac1 ) )
    ( format t "Test1 after macro.~%" )
    ( values )
)

( defun test2 ()
    ( format t "Test2 before macro.~%" )
    ( mac2 )
    ;( format t ( mac2 ) )
    ( format t "Test2 after macro.~%" )
    ( values )
)

( defun test1+2 ()
    ( format t "Test1+2 before macro.~%" )
    ( mac1 )
    ( mac2 )
    ;( format t ( mac1 ) )
    ;( format t ( mac2 ) )
    ( format t "Test1+2 after macro.~%" )
    ( values )
)

; old position of the mac2
( defmacro mac2 ()
    `( format t "Macro2~%" )
    ;"Macro2~%"
)

( defun test3 ()
    ( format t "Test3 before macro.~%" )
    ( mac2 )
    ;( format t ( mac2 ) )
    ( format t "Test3 after macro.~%" )
    ( values )
)
