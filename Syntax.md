Comments

	## This is a single-line comment
	
	#( This is a
	    multi-line
	    comment )#

Bool (implemented as java.lang.Boolean)

	true
	false

Int (implemented as java.lang.Long)

	1
	-20
	12345

Real (implemented as java.lang.Double)

	1.23
	-0.5

Char (implemented as java.lang.Character)

	#"a"
	#"\n"

Text (implemented as java.lang.String)

	"yolo"
	"Hello\tWorld!"

Forms

	(+ 1 2)
	(lambda [x] (foo 10 x))

Symbols

	foo     ## Unprefixed symbol (compiler will assume it's in the current module)
	bar;baz ## Prefixed symbol (compiler will assume it's in the module specified by the prefix)
	;fold   ## With just the semi-colon, compiler wil assume it's the same as lux;fold
	;;quux  ## With 2 semi-colons, it will get automatically prefixed with the current-module

Tags

	#Nil
	#lux;Cons
	#;Some
	#;;MyTag

Tuples

	[]
	["yolo" 10 true]

Variants (a.k.a. sum-types, a.k.a. discriminated unions)

	#Nil
	(#Cons [10 #Nil])

Records

	{#name "Lux" #awesome? true}