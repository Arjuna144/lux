Lux uses **signatures** and **structures** as the primary means of doing _polymorphism_ within the language.
Both concepts have types, data-structures & macros backing them up, to ensure that working with them is easy and productive.

## Signatures

Signatures are like interfaces that specify what data and operations must be in place for any given structure in order for the needs of the signature to be met.

Example:

	(defsig #export (Dict d)
	  (: (All [k v]
	        (-> k (d k v) (Maybe v)))
	     get)
	  (: (All [k v]
	        (-> k v (d k v) (d k v)))
	     put)
	  (: (All [k v]
	        (-> k (d k v) (d k v)))
	     remove))

Signatures must be named (e.g. `Dict`), can have a variable number of parameters (e.g. `d`) and can contain an arbitrary number of member definitions, which can correspond to _constant_ values or (very often) _functions_.

The members cannot contain any implementation code and can only specify what is needed of them (via their types).

The types of the members can make reference to any of the type-variables belonging to the signature (as is `d`).

## Structures

Structures are the implementations of signatures and contain all the actual code.
They must be complete (that is, they can't lack any of the members of their signatures) and they are bounded by their signature (that is, the can't contain more members than their signatures).

Another restriction that structures have is that they can't implement more than 1 signature at the same time.

Example:

	(defstruct #export PList/Dict (Dict PList)
	  (def (get k plist)
	    (let [(#PList [eq kvs]) plist]
	      (pl-get eq k kvs)))

	  (def (put k v plist)
	    (let [(#PList [eq kvs]) plist]
	      (#PList [eq (pl-put eq k v kvs)])))

	  (def (remove k plist)
	    (let [(#PList [eq kvs]) plist]
	      (#PList [eq (pl-remove eq k kvs)]))))

##### Note: It isn't necessary to provide type annotations for any of the members when defining a structure, as they're already covered by the signature of the structure itself.

## Macros

Besides `defsig` and `defstruct`, there are a few macros that are useful when working with signatures & structures.

#### using

`using` allows you to open a structure in a local scope in order to use the definitions within:

	(using List/Monoid
	  (foldL ++ unit xs))

The `Monoid` signature defines `++` (a.k.a. `mappend` in _Haskell_) and `unit` (a.k.a. `mempty` in _Haskell_). By opening it up, both can be using to perform (in this case) list concatenation.

#### open

`open` is just like `using`, with the difference that it works at the scope of modules and it installs the members of a structure as global definitions inside a module.

Also, in order to avoid name-clashing if multiple structures of the same signature are to be opened in the same module, it's possible to specify a (optional) prefix for the structure.

Examples:

	(open Text/Monoid "text:")

That expression would make available both `++` and `unit` under the names `text:++` and `text:unit`, respectively.

`open` can also be used from `import` statements at the beginning of modules, for ease and convenience:

	(;import (lux/data/text #as t #open ("text:" Text/Monoid Text/Eq)))

#### ::

`::` is a macro that's useful for accessing the members of structures piecemeal.
Not only can you extract the members, but you can invoke those that happen to be functions.
However, it must be noted that members accessed with `::` must be prefixed if they come from foreign modules, unlike when the same members are accessed through `using` or `open`.

Example:

	(:: List/Functor (F;map (foldL (:: Text/Monoid m;++) (:: Text/Monoid m;unit))
	                        (list (list "a" "b" "c")
	                              (list "d" "e" "f")
	                              (list "g" "h" "i"))))

##### Note: The code above assumes that the module `lux/control/functor` was imported under the alias "F" and the `lux/control/monoid` was imported under the alias "m".
