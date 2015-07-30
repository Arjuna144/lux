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

### Macro

	(deftype Macro
	  (-> (List Syntax) Compiler (Either Text (, Compiler (List Syntax)))))

## Macros

## Values