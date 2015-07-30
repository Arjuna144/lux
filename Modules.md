Lux files must end in the ".lux" extension and every file constitutes a **module**.

Unlike in other languages, where you must specify the name of the module inside the source file, Lux takes the name of the module from the name of the file itself and doesn't ask you for it.

An example of this would be a file named `foo.lux`. The compiler will just assume that all the code inside belongs to the `foo` module.

You can **import** other modules using the `import` macro, which can take the names of modules in either _absolute_ or _relative_ form.
`import` can also take extra options to make it specially easy to use foreign modules in your code.

This is an example of what can be done:

	(;import lux
		     (lux (codata (stream #as S))
		          (control monoid
		                   functor
		                   monad
		                   lazy
		                   comonad)
		          (data (bool #refer #all)
		                (bounded #refer (#only Bounded))
		                (dict #refer (#exclude PList))
		                (text #as t #open ("text:" Text/Monoid)))
		          (math #as m)
		          )
		     (../foo bar)
		     (./baz quux))

If you import a module but you don't give it an _alias_ (using `#as`) or _refer_ to anything in specific from it (using `#refer`), then all of its definitions will be imported as local definitions inside your module.

##### Note: This will only happen for those definitions that were _exported_ from the foreign modules.

If you _open_ a **structure** from a foreign module, then all of its members will also be imported as local definitions inside your module.

If you mention a module as part of the path for other sub-modules but you don't give it an alias, nor do you mention anything you want to refer or open from it, then the module won't be imported and it's name will only be used in the construction of the paths for the sub-modules mentioned.
An example of this is `lux/data`, in the previous code snippet; which is not going to be imported and is only useful for accessing lux/data/bool et al.
The same thing can be said for `../foo` and `./bar`

##### Note: You can't write any file-path that you want when importing, because that's not the way Lux import-paths work. In reality, the `import` macro is doing some trickery to translate the relative paths into absolute paths and the syntax it accepts is fairly limited (to avoid letting people do weird things with imports).
