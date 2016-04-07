( defun user-defined-variables ( &optional ( package :cl-user ) )
	( loop with package = ( find-package package )
		for symbol being the symbols of package
		when ( and ( eq ( symbol-package symbol ) package )
			( boundp symbol )
		)
		collect symbol
	)
)
