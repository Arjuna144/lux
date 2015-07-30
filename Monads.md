Lux offers great support for working with _functors_ and _monads_, 2 of the functional programming community's favorite abstractions.

The definitions of both signatures are in the `lux/control/functor` and `lux/control/monad` modules.

	(defsig #export (Functor f)
	  (: (All [a b]
	       (-> (-> a b) (f a) (f b)))
	     map))

	(defsig #export (Monad m)
	  (: (Functor m)
	     _functor)
	  (: (All [a]
	       (-> a (m a)))
	     wrap)
	  (: (All [a]
	       (-> (m (m a)) (m a)))
	     join))

##### Note: Notice how the `Monad` signature has an explicit `Functor` member. That is Lux's way of establishing _"inheritance"_ relationships.

`Monad`'s operations are `wrap` (Lux's analogue to Haskell's `return`) and `join` (a simpler alternative to Haskell's `>>=` (_bind_)).

To easily work with monads, Lux offers the `do` macro (Lux's implementation of Haskell's famous _do notation_):

	## Snippet from lux/meta/lux;macro-expand
	(do Lux/Monad
	  [expansion (macro args)
	   expansion' (M;map% Lux/Monad macro-expand expansion)
	   #let [full-expansion (:: List/Monad (M;join expansion'))]]
	  (M;wrap full-expansion))

Defining monads isn't hard and only requires you to implement structures for both the `Functor` and `Monad` signatures for your typess.
There are already several such implementations in the Lux standard library, in modules such as `lux/data/io`, `lux/data/list`, `lux/data/maybe`, and `lux/data/writer`.

Example from `lux/data/list`:

	(deftype (List a)
	  (| #;Nil
	     (#;Cons (, a (List a)))))

	(defstruct #export List/Functor (Functor List)
	  (def (F;map f ma)
	    (case ma
	      #;Nil            #;Nil
	      (#;Cons [a ma']) (#;Cons [(f a) (F;map f ma')]))))

	(defstruct #export List/Monad (Monad List)
	  (def M;_functor List/Functor)
	
	  (def (M;wrap a)
	    (#;Cons [a #;Nil]))
	
	  (def (M;join mma)
	    (using List/Monoid
	      (foldL ++ unit mma))))
