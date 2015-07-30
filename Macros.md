Macros are probably the most powerful tool that languages in the Lisp family can offer.

Lux is no exception, and its macros are enriched with information coming straight from the compiler itself.

However, in order to use this information, Lux macros are monadic and access to `Compiler` data must be done through the `Lux` monad.

Lux macros also have the characteristic that they may return an arbitrary number of syntax tokens; being required to return a list of tokens, rather than a single one, as most lisps do.

There are 3 modules that are important for working with macros:

## lux/meta/macro

Here is the `defmacro` macro, which allows you to easily define macros that take in a list of syntax tokens as an argument.

There are also a few functions for generating syntax tokens from raw data, such as `text$`, `symbol$`, and `form$`, among others.

Example:

	(defmacro #export (io tokens)
	  (case tokens
	    (\ (list value))
	    (let [blank (symbol$ ["" ""])]
	      (emit (list (` (_lux_lambda (~ blank) (~ blank) (~ value))))))
	
	    _
	    (fail "Wrong syntax for io")))

## lux/meta/lux

This module contains a variety of functions for interacting with the compiler.

From generating unique symbols with `gensym`, to querying the type of a global definition with `find-var-type`, up to performing macro expansions with `macro-expand` and `macro-expand-1`; `lux/meta/lux` offers all you need for your macro adventures.

Most importantly, `lux/meta/lux` contains the `Functor` and `Monad` structures for the `Lux` type, which come in very handy when writing complex macros (specially with the aid of `do` notation).

## lux/meta/syntax

This module is for those who have better things to do than parsing lists of syntax tokens by hand.

The `defsyntax` macro allows you to define macros using _monadic_ _parsers_ to extract the information for the input syntax tokens.

The term **monadic** **parser** might sound intimidating to you if you're not accustomed, but they are actually very simple and easy to work with, and make writing complex macros much more of a simple task.

Example:

	(defsyntax #export (definterface [name local-symbol^] [supers (tuple^ (*^ local-symbol^))] [members (*^ method-decl^)])
	  (let [members' (map (: (-> (, (List Text) Text (List Text) Text) Syntax)
	                         (lambda [member]
	                           (let [[modifiers name inputs output] member]
	                             (` ((~ (text$ name)) [(~@ (map text$ inputs))] (~ (text$ output)) [(~@ (map text$ modifiers))])))))
	                      members)]
	    (emit (list (` (_jvm_interface (~ (text$ name)) [(~@ (map text$ supers))]
	                     (~@ members')))))))

## Quasi-quotation

I didn't mention one key ingredient to having a successful career as a macro-writer: _quasi-quotation_

Quasi-quotes (a.k.a syntax-quotes, a.k.a. back-quotes) are a mechanism to write templates that represent the code you wish to generate, but having certain "holes" that you can use to plug-in syntax nodes that you generate on-the-fly inside your macros.

To use quasi-quotes, you must use the **`** macro (pronounced "quasi-quote").
It takes a piece of syntax which may contain unquoting expressions (forms beginning with `~`) and unquote-splicing expressions (forms beginning with `~@`).

Unquoting allows you to paste a single syntax node that is generated from the expression.
Unquote-splicing takes a list of nodes and just _splices_ them inside the larger parent syntax node.

Examples of both forms of _unquoting_ can be seen in the macro examples above.

## Quotation

A much simpler version of the quasi-quotation macro is the "quotation" macro `'`, which has no unquoting capabilities.

Examples:

	(let [sample "Hello, world!"]
	  (` (~ (text$ sample))))
	
	(let [sample "Hello, world!"]
	  (' (~ (text$ sample))))

The first example would yield a syntax token for the text `"Hello, world!"`, while the second example would yield a syntax token for the expression `(~ (text$ sample))`
