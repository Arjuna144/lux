Lux's _type-system_ is very similar to that of Haskell and ML, with a few key differences:

1. **There is no separation between regular types and modules/type-classes**: In Haskell, type-classes are not the same as regular types and you can't, for instance, write functions that take instances of type-classes directly, as arguments. You also can't parameterize type-classes with other type-classes and do other similar operations that you might easily do with regular types and type-constructors. In Lux, signatures (which are similar to ML's signatures) are types like any other and can be used in the same way as any type.
2. **Lux types are represented with Lux data-structures**: Most typed languages actually feature 2 languages in one: a _value-level language_ and a _type-level language_. Lux does away with this convention by having types be defined using data-structures like any you would write in your own programs. These data-structures get evaluated and incorporated by the compiler and used for all of the type-checking processes. Because of this, it's possible to generate types from functions and macros, and it's even possible to write functions that take and return types, possibly performing all kinds of transformations and queries.
3. **Lux types are structural, rather than nominal**: In Lux, what matters is not how the type is called, but what is it's _shape_, or what it _describes_.

## Type syntax

If you want to start using types, this is the syntax provided by the prelude (`lux` module):

#### Tuples:
Tuples are ordered groupings of elements.

Syntax:

	(, <type-1> <type-2> ...  <type-n>)
Example:

	(, Text Int)

##### Note: The _unit_ type is a special kind of tuple because it's the _empty tuple_ (specified by this type: `(,)`)

#### Variants (a.k.a. sum types, a.k.a. discriminated unions)
Variants represent valid alternatives for a given type.

Syntax:

	(| <case-1> <case-2> ... <case-n>)

Example:

	(| #True
	   #False)

	(| #None
	   (#Some Text))

##### Note: The _void_ type is a special kind of variant because it's the _empty variant_ (specified by this type: `(|)`)

#### Records
Records are just like tuples, with the exception that their members are named

Syntax:

	(& <tag-1> <type-1> <tag-2> <type-2> ... <tag-n> <type-n>)

Example:

	(& #name Text #age Int #country Text)

#### Primitives
While you can build types of any complexity using the combinators Lux offers (tuples, variants & records), you will also want to work with objects offered by the host platform, and these may be typed.
Primitive data-types can also be represented within Lux.

Syntax:

	(^ <type-name>)

Example:

	(^ java.lang.String)

#### Functions
The bread and butter of functional programming; function types in Lux bear a resemblance to how they look in Haskell (albeit with a lispier flavor):

Syntax

	(-> <arg-1> <arg-2> ... <arg-n> <return>)

Example:

	(-> Text Int (Maybe (, Text Text)))

#### Universal Quantification
The way to define polymorphic/generic types.

Syntax:

	(All [<var-1> <var-2> ... <var-n>] <type-exp>)

Example:

	(All [a b] (, (Eq a) (List (, a b))))

## Defining types

You can globally define types to reference them anywhere in your program. The `deftype` macro takes case of expanding into the syntax previously shown, and also provides some extra conveniences.

Examples:

	(deftype #export (IO a)
	  (-> (,) a))

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

##### Note: Yes, you just read the _type_ of types.

The `defsig` macro can also be used to define types, but with a different syntax (more useful for defining signatures). You can read more about it here: [Signatures & Structures](https://github.com/LuxLang/lux/wiki/Signatures-&-Structures)