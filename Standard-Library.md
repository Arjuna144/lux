# `lux`

## Types

### Bool

	(^ java.lang.Boolean)

### Int

	(^ java.lang.Long)

### Real

	(^ java.lang.Double)

### Char

	(^ java.lang.Character)

### Text

	(^ java.lang.String)

### Unit

	(,)

### Void

	(|)

### Ident

	(, Text Text)

### List

	(deftype (List a)
	  (| #Nil
	     (#Cons (, a (List a)))))

### Maybe

	(deftype (Maybe a)
	  (| #None
	     (#Some a)))

### Type

	(deftype #rec Type
	  (| (#DataT Text)
	     (#TupleT (List Type))
	     (#VariantT (List (, Text Type)))
	     (#RecordT (List (, Text Type)))
	     (#LambdaT (, Type Type))
	     (#BoundT Text)
	     (#VarT Int)
	     (#AllT (, (Maybe (List (, Text Type))) Text Text Type))
	     (#AppT (, Type Type))))

### Cursor

	(deftype Cursor
	  (, Text Int Int))

### Meta

	(deftype (Meta m v)
	  (| (#Meta (, m v))))

### Syntax

	(deftype #rec (Syntax w)
	  (Meta Cursor (| (#BoolS Bool)
	                  (#IntS Int)
	                  (#RealS Real)
	                  (#CharS Char)
	                  (#TextS Text)
	                  (#SymbolS (, Text Text))
	                  (#TagS (, Text Text))
	                  (#FormS (List Syntax))
	                  (#TupleS (List Syntax))
	                  (#RecordS (List (, Syntax Syntax))))))

### Either

	(deftype (Either l r)
	  (| (#Left l)
	     (#Right r)))

### Compiler

**???**

##### Note: Since Lux is still in early development, the structure of the compiler data-structure is subject to rapid and drastic changes between versions of Lux. Because of that, access to compiler data is best done through the functions provided in `lux/meta/lux`.

### Lux

	(deftype (Lux a)
	  (-> Compiler (Either Text (, Compiler a))))

### Macro

	(deftype Macro
	  (-> (List Syntax) (Lux (List Syntax))))

## Macros

### comment

	(comment 1 2 3 4) ## Same as not writing anything...

### lambda


	(def const
	  (lambda [x y] x))

	(def const
	  (lambda const [x y] x))

### let


	(let [x (foo bar)
	      y (baz quux)]
	  (op x y))

### $

	## Application of binary functions over variadic arguments.
	($ text:++ "Hello, " name ".\nHow are you?")
	=>
	(text:++ "Hello, " (text:++ name ".\nHow are you?"))

### |>

	## Piping
	(|> elems (map ->text) (interpose " ") (fold text:++ ""))
	=>
	(fold text:++ ""
		  (interpose " "
		             (map ->text elems)))

### if

	(if true
	  "Oh, yeah!"
	  "Aw hell naw!")

### ^

	## Macro to treat classes as types
	(^ java.lang.Object)

### ,

	## Tuples
	(, Text Int Bool)
	
	(,) ## The empty tuple, aka "unit"

### |

	(| #Yes #No)
	
	(|) ## The empty variant, aka "void"

### &

	## Records
	(& #name Text
	   #age Int
	   #alive? Bool)

### ->
	
	## Function types
	(-> Int Int Int) ## This is the type of a function that takes 2 Ints and returns an Int

### All

	## Universal quantification.
	(All List [a]
		 (| #Nil
		    (#Cons (, a (List a)))))

	## It must be explicit, unlike in Haskell.
	## Rank-n types will be possible as well as existential types
	(All [a]
	  (-> a a))

### type
Takes a type expression and returns it's representation as data-structure.

	(type (All [a] (Maybe (List a))))

### :

	## The type-annotation macro
	(: (List Int) (list 1 2 3))

### :!

	## The type-coercion macro
	(:! Dinosaur (list 1 2 3))

### deftype

	## The type-definition macro
	(deftype (List a)
	  (| #Nil
	     (#Cons (, a (List a)))))

### exec

	## Sequential execution of expressions (great for side-effects).
	(exec
	  (println! "#1")
	  (println! "#2")
	  (println! "#3")
	  "YOLO")

### def

	## Macro for definining global constants/functions.
	(def (rejoin-pair pair)
	  (-> (, Syntax Syntax) (List Syntax))
	  (let [[left right] pair]
		(list left right)))

### case

	## The pattern-matching macro.
	## Allows the usage of macros within the patterns to provide custom syntax.
	(case (: (List Int) (list 1 2 3))
	  (#Cons [x (#Cons [y (#Cons [z #Nil])])])
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(case (: (List Int) (list 1 2 3))
	  (\ (list x y z))
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(deftype Weekday
	  (| #Monday
	     #Tuesday
	     #Wednesday
	     #Thursday
	     #Friday
	     #Saturday
	     #Sunday))

	(def (weekend? day)
	  (-> Weekday Bool)
	  (case day
	     (\or #Saturday #Sunday)
	     true

	     _
	     false))

### \

	## It's a special macro meant to be used with case

### \or

	## It's a special macro meant to be used with case

### `

	## Quasi-quotation as a macro. Unquote (~) and unquote-splice (~@) must also be used as forms
	e.g.
	(` (def (~ name)
		 (lambda [(~@ args)]
		   (~ body))))

### sig

	## Not mean to be used directly. Prefer defsig

### struct

	## Not mean to be used directly. Prefer defstruct

### defsig

	## Definition of signatures ala ML
	(defsig #export (Ord a)
	  (: (-> a a Bool)
	     <)
	  (: (-> a a Bool)
	     <=)
	  (: (-> a a Bool)
	     >)
	  (: (-> a a Bool)
	     >=))

### defstruct

	## Definition of structures ala ML
	(defstruct #export Int/Ord (Ord Int)
	  (def (< x y)
	     (i< x y))
	  (def (<= x y)
	     (or (i< x y)
	         (i= x y)))
	  (def (> x y)
	     (i> x y))
	  (def (>= x y)
	     (or (i> x y)
	         (i= x y))))

### and

	(and true false true) ## => false

### or

	(or true false true) ## => true

### using

	## Opens up a structure and provides all the definitions as local variables.
	(using Int/Ord
	  (< 5 10))

## Values

### .
Function composition.

	(All [a b c]
	  (-> (-> b c) (-> a b) (-> a c)))

### i=
Int equality.

	(-> Int Int Bool)

### i>
Int greater-than.

	(-> Int Int Bool)

### i<
Int lesser-than.

	(-> Int Int Bool)

### r=
Real equality.

	(-> Real Real Bool)

### r>
Real greater-than.

	(-> Real Real Bool)

### r<
Real lesser-than.

	(-> Real Real Bool)

***

Long story short, you've got similar definitions for arithmetic on ints and reals:
`i+`, `i-`, `i*`, `i/`, `i%`, `i<=`, `i>=`, `r+`, `r-`, `r*`, `r/`, `r%`, `r<=`, `r>=`

***

### not

	(-> Bool Bool)

### id

	(All [a] (-> a a))

### flip

	(All [a b c]
	  (-> (-> a b c) (-> b a c)))

### curry

	(All [a b c]
	  (-> (-> (, a b) c)
	      (-> a b c)))

### uncurry

	(All [a b c]
	  (-> (-> a b c)
	      (-> (, a b) c)))

### complement

	(All [a] (-> (-> a Bool) (-> a Bool)))

# `lux/math`

## Values

### e

	Real

### pi

	Real

### cos

	(-> Real Real)

### sin

	(-> Real Real)

### tan

	(-> Real Real)

### acos

	(-> Real Real)

### asin

	(-> Real Real)

### atan

	(-> Real Real)

### cosh

	(-> Real Real)

### sinh

	(-> Real Real)

### tanh

	(-> Real Real)

### ceil

	(-> Real Real)

### floor

	(-> Real Real)

### exp

	(-> Real Real)

### log

	(-> Real Real)

### cbrt

	(-> Real Real)

### sqrt

	(-> Real Real)

### ->degrees

	(-> Real Real)

### ->radians

	(-> Real Real)

### round

	(-> Real Int)

### atan2

	(-> Real Real Real)

### pow

	(-> Real Real Real)