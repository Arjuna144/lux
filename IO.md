Lux is meant to be a language hosted on other platforms (currently only the JVM, but JavaScript interpreters and other VMs will see Lux's arrival in the future).

Because of this reason, all the mechanisms that Lux provides for doing I/O-based operations are going to rely on the resources that the host platform provides.

As many of us know, I/O is performed in an informal and unprincipled manner in many languages and platforms.
There is no way to declare the side-effects of computations (outside of exceptions, in some languages) and programs are often written with pervasive side-effects at different levels of the program.

Even many functional languages take that approach, often just out of a desire to avoid imposing unnecessary complexity on programmers who just want to _get things done_™.

Lux has a different perspective: **while unprincipled I/O allows you to code faster, in the long run, it becomes a source of errors, unexpected behavior and unreliability**.

For this reason, Lux takes Haskell's approach of doing I/O in a monadic fashion.

#### `lux/data/io`

This is the main module for all things I/O.
It hosts the `IO` type, plus its implementations of both the `Functor` and `Monad` signatures.
Another important member of this module is the `io` macro, which wraps whatever expression you give it inside the `IO` type, thereby making it easy to wrap side-effecting code from the host platform inside the `IO` type wrapper.

_Wait a minute!_, you might say. _If Lux is hosted on another platform, how can you have an `IO` type? Doesn't Haskell implement that as a low-level primitive?_

Haskell does rely on some compiler magic for it's `IO` type that Lux doesn't have, but that doesn't mean something can't be done about it.
The `IO` type, as Lux defines it, looks like this:

	(deftype #export (IO a)
	  (-> (,) a))

That is, a type `(IO a)` is a function from _unit_ (`(,)`) to `a`.
This way, the execution of side-effects is delayed until the evaluation of instances of `(IO a)`, so code using the host's I/O facilities can just be wrapped inside a function taking _unit_ and be executed whenever appropriate.

_________________________________________________________________________________________________________

Currently, there are few functions for doing I/O on the `lux/data/io` module, but that can easily be compensated by doing some host (_JVM_) interop to access the available I/O functionality (_always within the `io` macro, of course ;)_)