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
