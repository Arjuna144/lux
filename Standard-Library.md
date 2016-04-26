#lux
###Types
#####AST
The type of AST nodes for Lux syntax.
```
(deftype AST
  (Meta Cursor (AST' (Meta Cursor))))
```

#####AST'
```
(deftype (AST' w)
  (#BoolS Bool)
  (#IntS Int)
  (#RealS Real)
  (#CharS Char)
  (#TextS Text)
  (#SymbolS Ident)
  (#TagS Ident)
  (#FormS (List (w (AST' w))))
  (#TupleS (List (w (AST' w))))
  (#RecordS (List [(w (AST' w)) (w (AST' w))])))
```

#####Analysis
```
(deftype Analysis
  (Meta [Type Cursor] Void))
```

#####Bindings
```
(deftype (Bindings k v)
  {#counter Int
  #mappings (List [k v])})
```

#####Bool
```
(deftype Bool
  (^ java.lang.Boolean))
```

#####Bottom
The type of things whose type is unknown or undefined.
Useful for expressions that cause errors or other "extraordinary" conditions.
```
(deftype Bottom
  (All [a] a))
```

#####Char
```
(deftype Char
  (^ java.lang.Character))
```

#####Compiler
Represents the state of the Lux compiler during a run.
It's provided to macros during their invocation, so they can access compiler data.

Caveat emptor: Avoid fiddling with it, unless you know what you're doing.
```
(deftype Compiler
  {#info CompilerInfo
  #source Source
  #cursor Cursor
  #modules (List [Text Module])
  #envs (List Env)
  #type-vars (Bindings Int Type)
  #expected Type
  #seed Int
  #host Void})
```

#####CompilerInfo
```
(deftype CompilerInfo
  {#compiler-name Text
  #compiler-version Text
  #compiler-mode CompilerMode})
```

#####CompilerMode
```
(deftype CompilerMode
  (#Release Unit)
  (#Debug Unit)
  (#Eval Unit))
```

#####Cursor
Cursors are for specifying the location of AST nodes in Lux files during compilation.
```
(deftype Cursor
  {#module Text
  #line Int
  #column Int})
```

#####DefData
```
(deftype DefData
  [Type DefMeta Unit])
```

#####DefMeta
```
(deftype DefMeta
  (List [Ident DefMetaValue]))
```

#####DefMetaValue
```
(deftype #rec DefMetaValue
  (#BoolM Bool)
  (#IntM Int)
  (#RealM Real)
  (#CharM Char)
  (#TextM Text)
  (#IdentM Ident)
  (#ListM (List DefMetaValue))
  (#DictM (List [Text DefMetaValue])))
```

#####Either
```
(deftype (Either l r)
  (#Left l)
  (#Right r))
```

#####Env
```
(deftype Env
  {#name Text
  #inner-closures Int
  #locals (Bindings Text Analysis)
  #closure (Bindings Text Analysis)})
```

#####Ident
```
(deftype Ident
  [Text Text])
```

#####Int
```
(deftype Int
  (^ java.lang.Long))
```

#####List
```
(deftype (List a)
  #Nil
  (#Cons [a (List a)]))
```

#####Lux
Computations that can have access to the state of the compiler.
Those computations may also fail, or modify the state of the compiler.
```
(deftype (Lux a)
  (-> Compiler (Either Text [Compiler a])))
```

#####Macro
```
(deftype Macro
  (-> (List AST) (Lux (List AST))))
```

#####Maybe
```
(deftype (Maybe a)
  #None
  (#Some a))
```

#####Meta
The type of things that can have meta-data of arbitrary types.
```
(deftype (Meta m v)
  {#meta m
  #datum v})
```

#####Module
```
(deftype Module
  {#module-aliases (List [Text Text])
  #defs (List [Text DefData])
  #imports (List Text)
  #tags (List [Text Int (List Ident) Bool Type])
  #types (List [Text (List Ident) Bool Type])})
```

#####Real
```
(deftype Real
  (^ java.lang.Double))
```

#####Source
```
(deftype Source
  (List (Meta Cursor Text)))
```

#####Text
```
(deftype Text
  (^ java.lang.String))
```

#####Top
The type of things whose type doesn't matter.
It can be used to write functions or data-structures that can take, or return anything.
```
(deftype Top
  (Ex [a] a))
```

#####Type
This type represents the data-structures that are used to specify types themselves.
```
(deftype #rec Type
  (#DataT [Text (List Type)])
  (#VoidT Unit)
  (#UnitT Unit)
  (#SumT [Type Type])
  (#ProdT [Type Type])
  (#LambdaT [Type Type])
  (#BoundT Int)
  (#VarT Int)
  (#ExT Int)
  (#UnivQ [(List Type) Type])
  (#ExQ [(List Type) Type])
  (#AppT [Type Type])
  (#NamedT [Ident Type]))
```

#####Unit
```
(deftype Unit
  Unit)
```

#####Void
```
(deftype Void
  Void)
```

###Macros
#####$_
```
## Right-association for the application of binary functions over variadic arguments.
($_ Text:++ "Hello, " name ".\nHow are you?")

## =>
(Text:++ "Hello, " (Text:++ name ".\nHow are you?"))
```

#####&
```
## Tuple types:
(& Text Int Bool)

## The empty tuple, a.k.a. Unit.
(&)
```

#####'
```
## Quotation as a macro.
(' "YOLO")
```

#####->
```
## Function types:
(-> Int Int Int)

## This is the type of a function that takes 2 Ints and returns an Int.
```

#####:
```
## The type-annotation macro.
(:! (List Int) (list 1 2 3))
```

#####:!
```
## The type-coercion macro.
(:! Dinosaur (list 1 2 3))
```

#####::
```
## Allows accessing the value of a structure's member.
(:: Int/Show show)

## Also allows using that value as a function/method.
(:: Int/Show (show 123))
```

#####@ident
```
## Given a symbol or a tag, gives back a 2 tuple with the prefix and name parts, both as Text.
(@ident #lux;doc)

## =>
["lux" "doc"]
```

#####@type
```
## Takes a type expression and returns it's representation as data-structure.
(@type (All [a] (Maybe (List a))))
```

#####All
```
## Universal quantification.
(All [a]
  (-> a a))

## A name can be provided, to specify a recursive type.
(All List [a]
  (| Unit
     [a (List a)]))
```

#####Ex
```
## Existential quantification.
(Ex [a]
  [(Show a)
   a])

## A name can be provided, to specify a recursive type.
(Ex Self [a]
  [(Show a)
   a
   (List (Self a))])
```

#####Rec
```
## Parameter-less recursive types.
## A name has to be given to the whole type, to use it within it's body.
(Rec Self
  [Int (List Self)])
```

#####\
```
## Macro-expanding patterns.
## It's a special macro meant to be used with case.
(case (: (List Int) (list 1 2 3))
  (\ (list x y z))
  (#Some ($_ int:* x y z))

  _
  #None)
```

#####\open
```
## Same as the "open" macro, but meant to be used as a pattern-matching macro for generating local bindings.
## Can optionally take a "prefix" text for the generated local bindings.
(def #export (range (\open) from to)
  (All [a] (-> (Enum a) a a (List a)))
  (range' <= succ from to))
```

#####\or
```
## Or-patterns.
## It's a special macro meant to be used with case.
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
```

#####\slots
```
## Allows you to extract record members as local variables with the same names.
## For example:
(let [(\slots [#foo #bar #baz]) quux]
  (f foo bar baz))
```

#####\template
```
## It's similar to do-template, but meant to be used during pattern-matching.
(def #export (beta-reduce env type)
  (-> (List Type) Type Type)
  (case type
    (\template [<tag>]
     (<tag> members)
     (<tag> (List:map (beta-reduce env) members)))
    ([#;VariantT]
     [#;TupleT])

    (\template [<tag>]
     (<tag> left right)
     (<tag> (beta-reduce env left) (beta-reduce env right)))
    ([#;LambdaT]
     [#;AppT])

    (\template [<tag>]
     (<tag> env def)
     (case env
       #;Nil
       (<tag> env def)

       _
       type))
    ([#;UnivQ]
     [#;ExQ])

    (#;BoundT idx)
    (? type (at idx env))

    (#;NamedT name type)
    (beta-reduce env type)

    _
    type
    ))
```

#####\~
```
## Use global bindings with simple values, such as text, int, real, bool and char, in place for literals in patterns.
```

#####^
```
## Macro to treat classes as types.
(^ java.lang.Object)
```

#####_$
```
## Left-association for the application of binary functions over variadic arguments.
(_$ Text:++ "Hello, " name ".\nHow are you?")

## =>
(Text:++ (Text:++ "Hello, " name) ".\nHow are you?")
```

#####`
```
## Hygienic quasi-quotation as a macro. Unquote (~) and unquote-splice (~@) must also be used as forms.
## All unprefixed macros will receive their parent module's prefix if imported; otherwise will receive the prefix of the module on which the quasi-quote is being used.
(` (def (~ name)
     (lambda [(~@ args)]
       (~ body))))
```

#####`'
```
## Unhygienic quasi-quotation as a macro. Unquote (~) and unquote-splice (~@) must also be used as forms.
(`' (def (~ name)
      (lambda [(~@ args)]
        (~ body))))
```

#####and
```
(and true false true) ## => false
```

#####as-code
```
Creates code documentation, embedding text as comments and properly formatting the forms it's being given.

## For Example:
(as-code
 "Allows arbitrary looping, using the \"recur\" form to re-start the loop.
  Can be used in monadic code to create monadic loops."
 (loop [count 0
        x init]
   (if (not (< count 10))
     x
     (recur (inc count) (f x)))))
```

#####case
```
## The pattern-matching macro.
## Allows the usage of macros within the patterns to provide custom syntax.
(case (: (List Int) (list 1 2 3))
  (#Cons x (#Cons y (#Cons z #Nil)))
  (#Some ($_ int:* x y z))

  _
  #None)
```

#####case+
```
## Multi-level pattern matching.
## Useful in situations where the result of a branch depends on further refinements on the values being matched.
## For example:
(case+ (textlux;split(textlux;sizestatic) uri)
  (=> (#lux;Some[chunk uri']) (Text:= static chunk))
  (match-uri endpoint? parts' uri')

  _
  (#lux;Left(<> "Static part \\"#{static}\\" doesn't match URI: #{uri}")))

## Is the same as...
(case+ (textlux;split(textlux;sizestatic) uri)
  (=> (#lux;Some[chunk uri']) [(Text:= static chunk) true])
  (match-uri endpoint? parts' uri')

  _
  (#lux;Left(<> "Static part \\"#{static}\\" doesn't match URI: #{uri}")))
```

#####comment
```
## Throws away any code given to it.
## Great for commenting out code, while retaining syntax high-lightning and formatting in your text editor.
(comment 1 2 3 4)
```

#####cond
```
## Branching structures with multiple test conditions.
(cond (even? num) "even"
      (odd? num) "odd"
      ## else-branch
      "???")
```

#####def
```
## Defines global constants/functions.
(def (rejoin-pair pair)
  (-> [AST AST] (List AST))
  (let [[left right] pair]
    (list left right)))
```

#####defmacro
```
(defmacro #export (export tokens)
  (return (map (: (-> AST AST) (lambda [token] (` (;_lux_export (~ token))))) tokens)))
```

#####defsig
```
## Definition of signatures ala ML.
(defsig #export (Ord a)
  (: (Eq a)
     eq)
  (: (-> a a Bool)
     <)
  (: (-> a a Bool)
     <=)
  (: (-> a a Bool)
     >)
  (: (-> a a Bool)
     >=))
```

#####defstruct
```
## Definition of structures ala ML.
(defstruct #export Int/Ord (Ord Int)
  (def eq Int/Eq)
  (def (< x y)
    (_jvm_llt x y))
  (def (<= x y)
    (or (_jvm_llt x y)
        (_jvm_leq x y)))
  (def (> x y)
    (_jvm_lgt x y))
  (def (>= x y)
    (or (_jvm_lgt x y)
        (_jvm_leq x y))))
```

#####deftype
```
## The type-definition macro.
(deftype (List a)
  #Nil
  (#Cons (, a (List a))))
```

#####do-template
```
## By specifying a pattern (with holes), and the input data to fill those holes, repeats the pattern as many times as necessary.
(do-template [<name> <diff>]
             [(def #export <name>
                (-> Int Int)
                (i+ <diff>))]

             [inc 1]
             [dec -1])
```

#####exec
```
## Sequential execution of expressions (great for side-effects).
(exec
  (log! "#1")
  (log! "#2")
  (log! "#3")
  "YOLO")
```

#####get@
```
## Accesses the value of a record at a given tag.
(get@ #field my-record)
```

#####if
```
(if true
  "Oh, yeah!"
  "Aw hell naw!")
```

#####import
```
## Examples
(;import lux
         (lux (control (monad #as M #refer #all))
              (data (text #open ("text:" Text/Monoid))
                    (list #open ("list:" List/Monad))
                    maybe
                    (ident #open ("ident:" Ident/Show)))
              meta
              (meta ast))
         (.. (type #open ("" Type/Eq Type/Show))))

(;import lux
         (lux (control ["M" monad #*])
              (data [text "text:" Text/Monoid]
                    [list "list:" List/Monad]
                    maybe
                    [ident "ident:" Ident/Show])
              meta
              (meta ast))
         (.. [type "" Type/Eq Type/Show]))
```

#####lambda
```
## Syntax for creating functions.
## Allows for giving the function itself a name, for the sake of recursion.
(def const
  (All [a b] (-> a b a))
  (lambda [x y] x))

(def const
  (All [a b] (-> a b a))
  (lambda const [x y] x))
```

#####let
```
## Establishes lexical-bindings (local variables).
## Can (optionally) use pattern-matching macros when binding.
(let [x (foo bar)
      y (baz quux)]
  (op x y))
```

#####let%
```
## Controlled macro-expansion.
## Bind an arbitraty number of ASTs resulting from macro-expansion to local bindings.
## Wherever a binding appears, the bound ASTs will be spliced in there.
(testing "AST operations & structures"
  (let% [<tests> (do-template [<expr> <text> <pattern>]
                   [(compare <pattern> <expr>)
                    (compare <text> (:: astlux;AST/Show(show <expr>)))
                    (compare true (:: astlux;AST/Eq(= <expr> <expr>)))]

                   [(astlux;booltrue)                                     "true"       [["" -1 -1] (#lux;BoolStrue)]]
                   [(astlux;boolfalse)                                    "false"      [_ (#lux;BoolSfalse)]]
                   [(astlux;int123)                                       "123"        [_ (#lux;IntS123)]]
                   [(astlux;real123.0)                                    "123.0"      [_ (#lux;RealS123.0)]]
                   [(astlux;char#"\n")                                    "#\\"\\n\\"" [_ (#lux;CharS#"\n")]]
                   [(astlux;text"\\n")                                    "\\"\\n\\""  [_ (#lux;TextS"\\n")]]
                   [(astlux;tag["yolo" "lol"])                            "#yolo;lol"  [_ (#lux;TagS["yolo" "lol"])]]
                   [(astlux;symbol["yolo" "lol"])                         "yolo;lol"   [_ (#lux;SymbolS["yolo" "lol"])]]
                   [(astlux;form(list (astlux;booltrue) (astlux;int123)))     "(true 123)" (\ [_ (#lux;FormS(list [_ (#lux;BoolStrue)] [_ (#lux;IntS123)]))])]
                   [(astlux;tuple(list (astlux;booltrue) (astlux;int123)))    "[true 123]" (\ [_ (#lux;TupleS(list [_ (#lux;BoolStrue)] [_ (#lux;IntS123)]))])]
                   [(astlux;record(list [(astlux;booltrue) (astlux;int123)])) "{true 123}" (\ [_ (#lux;RecordS(list [[_ (#lux;BoolStrue)] [_ (#lux;IntS123)]]))])]
                   [(astlux;local-tag"lol")                               "#lol"       [_ (#lux;TagS["" "lol"])]]
                   [(astlux;local-symbol"lol")                            "lol"        [_ (#lux;SymbolS["" "lol"])]])]

    (test-all <tests>)))
```

#####list
```
## List-construction macro.
(list 1 2 3)
```

#####list&
```
## List-construction macro, with the last element being a tail-list.
## In other words, this macro prepends elements to another list.
(list& 1 2 3 (list 4 5 6))
```

#####loop
```
## Allows arbitrary looping, using the "recur" form to re-start the loop.
Can be used in monadic code to create monadic loops.
(loop [count 0
       x init]
  (if (not (< count 10))
    x
    (recur (inc count) (f x))))
```

#####open
```
## Opens a structure and generates a definition for each of its members (including nested members).
## For example:
(open Int/Number "i:")
## Will generate:
(def i:+ (:: Int/Number +))
(def i:- (:: Int/Number -))
(def i:* (:: Int/Number *))
...
```

#####or
```
(or true false true) ## => true
```

#####set@
```
## Sets the value of a record at a given tag.
(set@ #name "Lux" lang)
```

#####struct
```
Not meant to be used directly. Prefer "defstruct".
```

#####update@
```
## Modifies the value of a record at a given tag, based on some function (All [a] (-> a a)).
(update@ #age inc person)
```

#####using
```
## Opens up a structure and provides all the definitions as local bindings.
(using Int/Ord
  (< 5 10))
```

#####|
```
## Variant types:
(| Text Int Bool)

## The empty tuple, a.k.a. Void.
(|)
```

#####|>
```
## Piping macro.
(|> elems (map ->text) (interpose " ") (foldL Text:++ ""))

## =>
(foldL Text:++ ""
                  (interpose " "
                             (map ->text elems)))
```

#####|>.
```
## Similar to the piping macro, but rather than taking an initial object to work on, creates a function for taking it.
(|> (map ->text) (interpose " ") (foldL Text:++ ""))
## =>
(lambda [<something>]
  (foldL Text:++ ""
         (interpose " "
                    (map ->text <something>))))
```



###Values
#####.
```
Function composition.
```
`(All [a b c] (-> (-> b c) (-> a b) a c))`

#####dec
`(-> Int Int)`

#####(even? n)
`(-> Int Bool)`

#####(id x)
`(All [a] (-> a a))`

#####inc
`(-> Int Int)`

#####not
`(-> Bool Bool)`

#####(odd? n)
`(-> Int Bool)`

#####splice-helper
`(-> (List AST) (List AST) (List AST))`

#lux/cli
###Types
#####Option
```
(deftype (Option a)
  (-> (lux;List lux;Text) (lux/data/error;Error [(lux;List lux;Text) a])))
```



###Structs
#####Option/Applicative
`(lux/control/applicative;Applicative Option)`

#####Option/Functor
`(lux/control/functor;Functor Option)`

#####Option/Monad
`(lux/control/monad;Monad Option)`

###Values
#####(&^ optL optR)
`(All [a b] (-> (Option a) (Option b) (Option [a b])))`

#####(*^ opt inputs)
`(All [a] (-> (Option a) (Option (lux;List a))))`

#####(+^ opt)
`(All [a] (-> (Option a) (Option (lux;List a))))`

#####(?^ opt inputs)
`(All [a] (-> (Option a) (Option (lux;Maybe a))))`

#####(arg^ inputs)
`(Option lux;Text)`

#####(assert^ test message inputs)
`(-> lux;Bool lux;Text (Option lux;Unit))`

#####(default^ value option inputs)
`(All [a] (-> a (Option a) (Option a)))`

#####(end^ inputs)
`(Option lux;Unit)`

#####(flag^ names inputs)
`(-> (lux;List lux;Text) (Option lux;Bool))`

#####(not^ opt inputs)
`(All [a] (-> (Option a) (Option lux;Unit)))`

#####(option^ names inputs)
`(-> (lux;List lux;Text) (Option lux;Text))`

#####(parse^ parser option inputs)
`(All [a] (-> (-> lux;Text (lux/data/error;Error a)) (Option lux;Text) (Option a)))`

#####(run-cli opt inputs)
`(All [a] (-> (Option a) (lux;List lux;Text) (lux/data/error;Error a)))`

#####(|^ optL optR inputs)
`(All [a b] (-> (Option a) (Option b) (Option (| a b))))`

#####(||^ opts inputs)
`(All [a] (-> (lux;List (Option a)) (Option a)))`

#lux/codata/cont
###Types
#####Cont
```
(deftype (Cont a)
  (All [b] (-> (-> a b) b)))
```

###Macros
#####...
```
## Delays the evaluation of an expression, by wrapping it in a continuation 'thunk'.
(... (some-computation some-input))
```

###Structs
#####Cont/Applicative
`(lux/control/applicative;Applicative Cont)`

#####Cont/Functor
`(lux/control/functor;Functor Cont)`

#####Cont/Monad
`(lux/control/monad;Monad Cont)`

###Values
#####(! thunk)
`(All [a] (-> (Cont a) a))`

#####(call/cc f)
`(All [a b c] (Cont (-> a (Cont b c)) (Cont a c)))`

#####(run-cont l k)
`(All [a b] (-> (Cont a b) (-> a b) b))`

#lux/codata/function




###Structs
#####Function/Category
`(lux/control/category;Category (All [a b] (-> a b)))`

#####Function/Monoid
`(lux/control/monoid;Monoid (All [a] (-> a a)))`

###Values
#####(const x y)
`(All [a b] (-> a b a))`

#####(flip f)
`(All [a b c] (-> (-> a b c) b a c))`

#lux/codata/io
###Types
#####IO
```
(deftype (IO a)
  (-> lux;Void a))
```

###Macros
#####io
```
## Delays the evaluation of an expression, by wrapping it in an IO 'thunk'.
## Great for wrapping side-effecting computations (which won't be performed until the IO is "run").
(io (exec
      (log! msg)
      "Some value..."))
```

###Structs
#####IO/Applicative
`(lux/control/applicative;Applicative IO)`

#####IO/Functor
`(lux/control/functor;Functor IO)`

#####IO/Monad
`(lux/control/monad;Monad IO)`

###Values
#####(run-io action)
`(All [a] (-> (IO a) a))`

#lux/codata/reader
###Types
#####Reader
```
(deftype (Reader r a)
  (-> r a))
```



###Structs
#####Reader/Applicative
`(All [a] (lux/control/applicative;Applicative (Reader a)))`

#####Reader/Functor
`(All [a] (lux/control/functor;Functor (Reader a)))`

#####Reader/Monad
`(All [a] (lux/control/monad;Monad (Reader a)))`



#lux/codata/state
###Types
#####State
```
(deftype (State s a)
  (-> s [s a]))
```



###Structs
#####State/Applicative
`(All [a] (lux/control/applicative;Applicative (State a)))`

#####State/Functor
`(All [a] (lux/control/functor;Functor (State a)))`

#####State/Monad
`(All [a] (lux/control/monad;Monad (State a)))`

###Values
#####(run-state state action)
`(All [a b] (-> a (State a b) b))`

#lux/codata/stream
###Types
#####Stream
```
(deftype (Stream a)
  (lux/codata/cont;Cont [a (Stream a)]))
```

###Macros
#####\stream&
```
## Allows destructuring of streams in pattern-matching expressions.
## Caveat emptor: Only use it for destructuring, and not for testing values within the streams.
(let [(\stream& x y z _tail) (some-stream-func 1 2 3)]
  (func x y z))
```

###Structs
#####Stream/CoMonad
`(lux/control/comonad;CoMonad Stream)`

#####Stream/Functor
`(lux/control/functor;Functor Stream)`

###Values
#####(at idx s)
`(All [a] (-> lux;Int (Stream a) a))`

#####(cycle xs)
`(All [a] (-> (lux;List a) (lux;Maybe (Stream a))))`

#####(drop det xs)
`(All [a] (-> lux;Int (Stream a) (Stream a)))`

#####(drop-while det xs)
`(All [a] (-> (-> a lux;Bool) (Stream a) (Stream a)))`

#####(filter p xs)
`(All [a] (-> (-> a lux;Bool) (Stream a) (Stream a)))`

#####(head s)
`(All [a] (-> (Stream a) a))`

#####(iterate f x)
`(All [a] (-> (-> a a) a (Stream a)))`

#####(partition p xs)
`(All [a] (-> (-> a lux;Bool) (Stream a) [(Stream a) (Stream a)]))`

#####(repeat x)
`(All [a] (-> a (Stream a)))`

#####(split det xs)
`(All [a] (-> lux;Int (Stream a) [(lux;List a) (Stream a)]))`

#####(split-with det xs)
`(All [a] (-> (-> a lux;Bool) (Stream a) [(lux;List a) (Stream a)]))`

#####(tail s)
`(All [a] (-> (Stream a) (Stream a)))`

#####(take det xs)
`(All [a] (-> lux;Int (Stream a) (lux;List a)))`

#####(take-while det xs)
`(All [a] (-> (-> a lux;Bool) (Stream a) (lux;List a)))`

#####(unfold step init)
`(All [a b] (-> (-> a [a b]) a (Stream b)))`

#lux/concurrency/async
###Types
#####Async
Represents values produced by asynchronous computations (unlike IO, which is synchronous).
```
(deftype Async
  (All [a] (^ lux.concurrency.async.JvmAsync a)))
```

###Macros
#####async
```
## Makes an uninitialized Async.
(async)
```

###Structs
#####Async/Applicative
`(lux/control/applicative;Applicative Async)`

#####Async/Functor
`(lux/control/functor;Functor Async)`

#####Async/Monad
`(lux/control/monad;Monad Async)`

###Values
#####(&! left right)
`(All [a b] (-> (Async a) (Async b) (Async [a b])))`

#####(delay time value)
`(All [a] (-> lux;Int a (Async a)))`

#####(future computation)
`(All [a] (-> (lux/codata/io;IO a) (Async a)))`

#####(query async)
`(All [a] (-> (Async a) (lux;Maybe a)))`

#####(resolve value async)
`(All [a] (-> a (Async a) (lux/codata/io;IO lux;Bool)))`

#####(time-out! time async)
`(All [a] (-> lux;Int (Async a) (Async (lux;Maybe a))))`

#####(wait time)
`(-> lux;Int (Async lux;Unit))`

#####(|! left right)
`(All [a b] (-> (Async a) (Async b) (Async (lux;Either a b))))`

#lux/concurrency/frp
###Types
#####Chan
```
(deftype (Chan a)
  (lux/concurrency/async;Async (lux;Maybe [a (Chan a)])))
```

###Macros
#####chan
```
## Makes an uninitialized Chan.
(chan)
```

###Structs
#####Chan/Applicative
`(lux/control/applicative;Applicative Chan)`

#####Chan/Functor
`(lux/control/functor;Functor Chan)`

#####Chan/Monad
`(lux/control/monad;Monad Chan)`

###Values
#####(->chan !x)
`(All [a] (-> (lux/concurrency/async;Async a) (Chan a)))`

#####(->list xs)
`(All [a] (-> (Chan a) (lux/concurrency/async;Async (lux;List a))))`

#####(close chan)
`(All [a] (-> (Chan a) (lux/codata/io;IO lux;Bool)))`

#####(filter p xs)
`(All [a] (-> (-> a lux;Bool) (Chan a) (Chan a)))`

#####(foldL f init xs)
`(All [a b] (-> (-> a b a) a (Chan b) (lux/concurrency/async;Async a)))`

#####(merge xss)
`(All [a] (-> (lux;List (Chan a)) (Chan a)))`

#####(no-dups eq xs)
`(All [a] (-> (lux/control/eq;Eq a) (Chan a) (Chan a)))`

#####(pipe input output)
`(All [a] (-> (Chan a) (Chan a) (lux/concurrency/async;Async lux;Unit)))`

#####(poll time action)
`(All [a] (-> lux;Int (lux/codata/io;IO a) (Chan a)))`

#####(write value chan)
`(All [a] (-> a (Chan a) (lux/codata/io;IO (lux;Maybe (Chan a)))))`

#lux/control/applicative
###Types
#####Applicative
```
(defsig (Applicative f)
  (: (lux/control/functor;Functor f)
     functor)
  (: (All [b] (-> a (f a)))
     wrap)
  (: (All [b c] (-> (f (-> a b)) (f a) (f b)))
     apply))
```







#lux/control/bounded
###Types
#####Bounded
```
(defsig (Bounded a)
  (: a
     top)
  (: a
     bottom))
```







#lux/control/category
###Types
#####Category
```
(defsig (Category cat)
  (: (All [b] (cat a a))
     id)
  (: (All [b c d] (-> (cat b c) (cat a b) (cat a c)))
     .))
```







#lux/control/comonad
###Types
#####CoMonad
```
(defsig (CoMonad w)
  (: (lux/control/functor;Functor w)
     functor)
  (: (All [b] (-> (w a) a))
     unwrap)
  (: (All [b] (-> (w a) (w (w a))))
     split))
```

###Macros
#####be
```
## A co-monadic parallel to the "do" macro.
(let [square (lambda [n] (i:* n n))]
  (be Stream/CoMonad
    [inputs (iterate inc 2)]
    (square (head inputs))))
```





#lux/control/enum
###Types
#####Enum
```
(defsig (Enum e)
  (: (lux/control/ord;Ord e)
     ord)
  (: (-> e e)
     succ)
  (: (-> e e)
     pred))
```





###Values
#####(range (\open) from to)
`(All [a] (-> (Enum a) a a (lux;List a)))`

#lux/control/eq
###Types
#####Eq
```
(defsig (Eq a)
  (: (-> a a lux;Bool)
     =))
```







#lux/control/fold
###Types
#####Fold
```
(defsig (Fold F)
  (: (All [b c] (-> (-> a b a) a (F b) a))
     foldL)
  (: (All [b c] (-> (-> b a a) a (F b) a))
     foldR))
```





###Values
#####(empty? fold xs)
`(All [a b] (-> (Fold a) (a b) lux;Bool))`

#####(fold (\open) (\open) xs)
`(All [a b] (-> (lux/control/monoid;Monoid b) (Fold a) (a b) b))`

#####(member? (\open) (\open) xs x)
`(All [a b] (-> (lux/control/eq;Eq b) (Fold a) (a b) b lux;Bool))`

#####(size (\open) xs)
`(All [a b] (-> (Fold a) (a b) lux;Int))`

#lux/control/functor
###Types
#####Functor
```
(defsig (Functor f)
  (: (All [b c] (-> (-> a b) (f a) (f b)))
     map))
```







#lux/control/hash
###Types
#####Hash
```
(defsig (Hash a)
  (: (-> a lux;Int)
     hash))
```







#lux/control/monad
###Types
#####Monad
```
(defsig (Monad m)
  (: (lux/control/applicative;Applicative m)
     applicative)
  (: (All [b] (-> (m (m a)) (m a)))
     join))
```

###Macros
#####do
```
## Macro for easy concatenation of monadic operations.
(do Maybe/Monad
  [y (f1 x)
   z (f2 z)]
  (wrap (f3 z)))
```

#####|>%
```
## Just like the piping macro, but for monadic operations.
(|>% IO/Monad
     (make-chat-manager conn)
     (open-chat user-id)
     (send-message message))
```



###Values
#####(foldL% monad f init xs)
`(All [a b c] (-> (Monad a) (-> b c (a b)) b (lux;List c) (a b)))`

#####(map% monad f xs)
`(All [a b c] (-> (Monad a) (-> b (a c)) (lux;List b) (a (lux;List c))))`

#####(seq% monad xs)
`(All [a b] (-> (Monad a) (lux;List (a b)) (a (lux;List b))))`

#lux/control/monoid
###Types
#####Monoid
```
(defsig (Monoid a)
  (: a
     unit)
  (: (-> a a a)
     ++))
```







#lux/control/number
###Types
#####Number
```
(defsig (Number n)
  (: (lux/control/ord;Ord n)
     ord)
  (: (-> n n n)
     +)
  (: (-> n n n)
     -)
  (: (-> n n n)
     *)
  (: (-> n n n)
     /)
  (: (-> n n n)
     %)
  (: (-> n n)
     negate)
  (: (-> n n)
     signum)
  (: (-> n n)
     abs)
  (: (-> lux;Int n)
     from-int))
```







#lux/control/ord
###Types
#####Ord
```
(defsig (Ord a)
  (: (lux/control/eq;Eq a)
     eq)
  (: (-> a a lux;Bool)
     <)
  (: (-> a a lux;Bool)
     <=)
  (: (-> a a lux;Bool)
     >)
  (: (-> a a lux;Bool)
     >=))
```





###Values
#####(max ord x y)
`(All [a] (-> (Ord a) a a a))`

#####(min ord x y)
`(All [a] (-> (Ord a) a a a))`

#####(ord$ eq <)
`(All [a] (-> (lux/control/eq;Eq a) (-> a a lux;Bool) (Ord a)))`

#lux/control/read
###Types
#####Read
```
(defsig (Read a)
  (: (-> lux;Text (lux/data/error;Error a))
     read))
```







#lux/control/show
###Types
#####Show
```
(defsig (Show a)
  (: (-> a lux;Text)
     show))
```







#lux/data/bool




###Structs
#####And/Monoid
`(lux/control/monoid;Monoid lux;Bool)`

#####Bool/Eq
`(lux/control/eq;Eq lux;Bool)`

#####Bool/Read
`(lux/control/read;Read lux;Bool)`

#####Bool/Show
`(lux/control/show;Show lux;Bool)`

#####Or/Monoid
`(lux/control/monoid;Monoid lux;Bool)`

###Values
#####comp
`(All [a] (-> (-> a lux;Bool) a lux;Bool))`

#lux/data/char




###Structs
#####Char/Eq
`(lux/control/eq;Eq lux;Char)`

#####Char/Ord
`(lux/control/ord;Ord lux;Char)`

#####Char/Show
`(lux/control/show;Show lux;Char)`

###Values
#####(->text c)
`(-> lux;Char lux;Text)`

#####(white-space? c)
`(-> lux;Char lux;Bool)`

#lux/data/error
###Types
#####Error
```
(deftype (Error a)
  (lux;Either lux;Text a))
```



###Structs
#####Error/Applicative
`(lux/control/applicative;Applicative Error)`

#####Error/Functor
`(lux/control/functor;Functor Error)`

#####Error/Monad
`(lux/control/monad;Monad Error)`



#lux/data/id
###Types
#####Id
```
(deftype (Id a)
  a)
```



###Structs
#####Id/Applicative
`(lux/control/applicative;Applicative Id)`

#####Id/CoMonad
`(lux/control/comonad;CoMonad Id)`

#####Id/Functor
`(lux/control/functor;Functor Id)`

#####Id/Monad
`(lux/control/monad;Monad Id)`



#lux/data/ident




###Structs
#####Ident/Eq
`(lux/control/eq;Eq lux;Ident)`

#####Ident/Show
`(lux/control/show;Show lux;Ident)`

###Values
#####(module [left right])
`(-> lux;Ident lux;Text)`

#####(name [left right])
`(-> lux;Ident lux;Text)`

#lux/data/list


###Macros
#####zip
```
## Create list zippers with the specified number of input lists.
(def #export zip2 (zip 2))

(def #export zip3 (zip 3))

((zip 3) xs ys zs)
```

###Structs
#####List/Applicative
`(lux/control/applicative;Applicative lux;List)`

#####(List/Eq (\open "a:"))
`(All [a] (-> (lux/control/eq;Eq a) (lux/control/eq;Eq (lux;List a))))`

#####List/Fold
`(lux/control/fold;Fold lux;List)`

#####List/Functor
`(lux/control/functor;Functor lux;List)`

#####List/Monad
`(lux/control/monad;Monad lux;List)`

#####List/Monoid
`(All [a] (lux/control/monoid;Monoid (lux;List a)))`

###Values
#####(any? p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) lux;Bool))`

#####(as-pairs xs)
`(All [a] (-> (lux;List a) (lux;List [a a])))`

#####(at i xs)
`(All [a] (-> lux;Int (lux;List a) (lux;Maybe a)))`

#####concat
`(All [a] (-> (lux;List (lux;List a)) (lux;List a)))`

#####(drop n xs)
`(All [a] (-> lux;Int (lux;List a) (lux;List a)))`

#####(drop-while p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) (lux;List a)))`

#####(empty? xs)
`(All [a] (-> (lux;List a) lux;Bool))`

#####(every? p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) lux;Bool))`

#####(filter p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) (lux;List a)))`

#####(interpose sep xs)
`(All [a] (-> a (lux;List a) (lux;List a)))`

#####(iterate f x)
`(All [a] (-> (-> a (lux;Maybe a)) a (lux;List a)))`

#####(member? eq xs x)
`(All [a] (-> (lux/control/eq;Eq a) (lux;List a) a lux;Bool))`

#####(partition p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) [(lux;List a) (lux;List a)]))`

#####(range from to)
`(-> lux;Int lux;Int (lux;List lux;Int))`

#####(repeat n x)
`(All [a] (-> lux;Int a (lux;List a)))`

#####(reverse xs)
`(All [a] (-> (lux;List a) (lux;List a)))`

#####(size list)
`(All [a] (-> (lux;List a) lux;Int))`

#####(some f xs)
`(All [a b] (-> (-> a (lux;Maybe b)) (lux;List a) (lux;Maybe b)))`

#####(sort < xs)
`(All [a] (-> (-> a a lux;Bool) (lux;List a) (lux;List a)))`

#####(split n xs)
`(All [a] (-> lux;Int (lux;List a) [(lux;List a) (lux;List a)]))`

#####(split-with p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) [(lux;List a) (lux;List a)]))`

#####(take n xs)
`(All [a] (-> lux;Int (lux;List a) (lux;List a)))`

#####(take-while p xs)
`(All [a] (-> (-> a lux;Bool) (lux;List a) (lux;List a)))`

#####zip2
`(All [a b] (-> (lux;List a) (lux;List b) (lux;List [a b])))`

#####zip3
`(All [a b c] (-> (lux;List a) (lux;List b) (lux;List c) (lux;List [a b c])))`

#lux/data/maybe




###Structs
#####Maybe/Applicative
`(lux/control/applicative;Applicative lux;Maybe)`

#####Maybe/Functor
`(lux/control/functor;Functor lux;Maybe)`

#####Maybe/Monad
`(lux/control/monad;Monad lux;Maybe)`

#####Maybe/Monoid
`(All [a] (lux/control/monoid;Monoid (lux;Maybe a)))`

###Values
#####(? else maybe)
`(All [a] (-> a (lux;Maybe a) a))`

#lux/data/number


###Macros
#####@bin


#####@hex


#####@oct


###Structs
#####Int/Bounded
`(lux/control/bounded;Bounded lux;Int)`

#####Int/Enum
`(lux/control/enum;Enum lux;Int)`

#####Int/Eq
`(lux/control/eq;Eq lux;Int)`

#####Int/Number
`(lux/control/number;Number lux;Int)`

#####Int/Ord
`(lux/control/ord;Ord lux;Int)`

#####Int/Read
`(lux/control/read;Read lux;Int)`

#####Int/Show
`(lux/control/show;Show lux;Int)`

#####IntAdd/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMax/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMin/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMul/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####Real/Bounded
`(lux/control/bounded;Bounded lux;Real)`

#####Real/Eq
`(lux/control/eq;Eq lux;Real)`

#####Real/Number
`(lux/control/number;Number lux;Real)`

#####Real/Ord
`(lux/control/ord;Ord lux;Real)`

#####Real/Read
`(lux/control/read;Read lux;Real)`

#####Real/Show
`(lux/control/show;Show lux;Real)`

#####RealAdd/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMax/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMin/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMul/Monoid
`(lux/control/monoid;Monoid lux;Real)`

###Values
#####(->int n)
`(-> lux;Real lux;Int)`

#####(->real n)
`(-> lux;Int lux;Real)`

#####(binary->int repr)
`(-> lux;Text (lux/data/error;Error lux;Int))`

#####(hex->int repr)
`(-> lux;Text (lux/data/error;Error lux;Int))`

#####i-max
`(-> lux;Int lux;Int lux;Int)`

#####i-min
`(-> lux;Int lux;Int lux;Int)`

#####(int->binary value)
`(-> lux;Int lux;Text)`

#####(int->hex value)
`(-> lux;Int lux;Text)`

#####(int->octal value)
`(-> lux;Int lux;Text)`

#####(octal->int repr)
`(-> lux;Text (lux/data/error;Error lux;Int))`

#####r-max
`(-> lux;Real lux;Real lux;Real)`

#####r-min
`(-> lux;Real lux;Real lux;Real)`

#lux/data/product
###Types
#####Writer
```
(deftype (Writer l a)
  [l a])
```



###Structs
#####(Writer/Applicative mon)
`(All [a] (-> (lux/control/monoid;Monoid a) (lux/control/applicative;Applicative (Writer a))))`

#####Writer/Functor
`(All [a] (lux/control/functor;Functor (Writer a)))`

#####(Writer/Monad mon)
`(All [a] (-> (lux/control/monoid;Monoid a) (lux/control/monad;Monad (Writer a))))`

###Values
#####(curry f)
`(All [a b c] (-> (-> [a b] c) a b c))`

#####(left xy)
`(All [a b] (-> [a b] a))`

#####(log x)
`(All [a] (-> a (Writer a lux;Unit)))`

#####(right xy)
`(All [a b] (-> [a b] b))`

#####(swap xy)
`(All [a b] (-> [a b] [b a]))`

#####(uncurry f)
`(All [a b c] (-> (-> a b c) [a b] c))`

#lux/data/sum






###Values
#####(either f g e)
`(All [a b c] (-> (-> a c) (-> b c) (| a b) c))`

#####(left value)
`(All [a b] (-> a (| a b)))`

#####(lefts es)
`(All [a b] (-> (lux;List (| a b)) (lux;List a)))`

#####(partition xs)
`(All [a b] (-> (lux;List (| a b)) [(lux;List a) (lux;List b)]))`

#####(right value)
`(All [a b] (-> b (| a b)))`

#####(rights es)
`(All [a b] (-> (lux;List (| a b)) (lux;List b)))`

#lux/data/text


###Macros
#####<>
```
## Text interpolation as a macro.
(<> "Static part \\"#{static}\\" doesn't match URI: #{uri}")
```

###Structs
#####Text/Eq
`(lux/control/eq;Eq lux;Text)`

#####Text/Monoid
`(lux/control/monoid;Monoid lux;Text)`

#####Text/Ord
`(lux/control/ord;Ord lux;Text)`

#####Text/Show
`(lux/control/show;Show lux;Text)`

###Values
#####(at idx x)
`(-> lux;Int lux;Text (lux;Maybe lux;Char))`

#####(concat texts)
`(-> (lux;List lux;Text) lux;Text)`

#####(contains? subtext text)
`(-> lux;Text lux;Text lux;Bool)`

#####(empty? text)
`(-> lux;Text lux;Bool)`

#####(ends-with? postfix x)
`(-> lux;Text lux;Text lux;Bool)`

#####(index-of pattern x)
`(-> lux;Text lux;Text (lux;Maybe lux;Int))`

#####(index-of' pattern from x)
`(-> lux;Text lux;Int lux;Text (lux;Maybe lux;Int))`

#####(join-with sep texts)
`(-> lux;Text (lux;List lux;Text) lux;Text)`

#####(last-index-of pattern x)
`(-> lux;Text lux;Text (lux;Maybe lux;Int))`

#####(last-index-of' pattern from x)
`(-> lux;Text lux;Int lux;Text (lux;Maybe lux;Int))`

#####(lower-case x)
`(-> lux;Text lux;Text)`

#####(replace pattern value template)
`(-> lux;Text lux;Text lux;Text lux;Text)`

#####(replace-once pattern value template)
`(-> lux;Text lux;Text lux;Text lux;Text)`

#####(size x)
`(-> lux;Text lux;Int)`

#####(split at x)
`(-> lux;Int lux;Text (lux;Maybe [lux;Text lux;Text]))`

#####(split-all-with token sample)
`(-> lux;Text lux;Text (lux;List lux;Text))`

#####split-lines
`(-> lux;Text (lux;List lux;Text))`

#####(split-with token sample)
`(-> lux;Text lux;Text (lux;Maybe [lux;Text lux;Text]))`

#####(starts-with? prefix x)
`(-> lux;Text lux;Text lux;Bool)`

#####(sub from to x)
`(-> lux;Int lux;Int lux;Text (lux;Maybe lux;Text))`

#####(sub' from x)
`(-> lux;Int lux;Text (lux;Maybe lux;Text))`

#####(trim x)
`(-> lux;Text lux;Text)`

#####(upper-case x)
`(-> lux;Text lux;Text)`

#lux/hack


###Macros
#####:!!
```
## Coerces the given expression to the type of whatever is expected.
(: Dinosaur (:!! (list 1 2 3)))
```

#####error
```
## Causes an error, with the given error message.
(error "OH NO!")
```

#####undefined
```
## Meant to be used as a stand-in for functions with undefined implementations.
(def (square x)
  (-> Int Int)
  (undefined))
```



###Values
#####(log! message)
`(-> lux;Text lux;Unit)`

#lux/host
###Types
#####Array
```
(deftype (Array a)
  (^ #Array a))
```

###Macros
#####!!!
```
## Takes a (Maybe ObjectType) and return a ObjectType.
## A #;None would gets translated in to a (null).
## Takes a (potentially null) object pointer and creates a (Maybe ObjectType) for it.
(!!! (??? (: java.lang.Thread (null))))

## =>
(null)

(!!! (??? "YOLO"))

## =>
## YOLO
```

#####???
```
## Takes a (potentially null) object pointer and creates a (Maybe ObjectType) for it.
(??? (: java.lang.Thread (null)))

## =>
#lux;None

(??? "YOLO")

## =>
(#lux;Some"YOLO")
```

#####array
```
## Create an array of the given type, with the given size.
(array Object 10)
```

#####array-length
```
## Gives the length of an array.
(array-length my-array)
```

#####array-load
```
## Loads an element from an array.
(array-load 10 my-array)
```

#####array-store
```
## Stores an element into an array.
(array-store 10 my-object my-array)
```

#####defclass
```
## Allows defining JVM classes in Lux code.
## For example:
(defclass #final (JvmAsync A) []

  (#private resolved boolean)
  (#private datum A)
  (#private waitingList (java.util.List lux.Function))

  (#public new [] [] []
           (exec (:= .resolved false)
             (:= .waitingList (ArrayList::new []))
             []))
  (#public resolve [] [(value A)] boolean
           (let [container (.new! [])]
             (locking _jvm_this
               (if .resolved
                 false
                 (exec (:= .datum value)
                   (:= .resolved true)
                   (let [sleepers .waitingList
                         sleepers-count (java.util.List::size [] sleepers)]
                     (map (lambda [idx]
                            (let [sleeper (java.util.List::get [(l2i idx)] sleepers)]
                              (Executor::execute [(@runnable (lux.Function::apply [(:! Object value)] sleeper))]
                                                 executor)))
                          (range 0 (dec (i2l sleepers-count)))))
                   (:= .waitingList (null))
                   true)))))
  (#public poll [] [] A
           .datum)
  (#public wasResolved [] [] boolean
           (locking _jvm_this
             .resolved))
  (#public waitOn [] [(callback lux.Function)] void
           (locking _jvm_this
             (exec (if .resolved
                     (lux.Function::apply [(:! Object .datum)] callback)
                     (:! Object (java.util.List::add [callback] .waitingList)))
               [])))
  (#public #static make [A] [(value A)] (lux.concurrency.async.JvmAsync A)
           (let [container (.new! [])]
             (exec (.resolve! (:! (^ lux.concurrency.async.JvmAsync [Unit]) container) [(:! Unit value)])
               container))))

## The vector corresponds to parent interfaces.
## An optional super-class can be specified before the vector. If not specified, java.lang.Object will be assumed.
## Fields and methods defined in the class can be used with special syntax.
## For example:
## .resolved, for accessing the "resolved" field.
## (:= .resolved true) for modifying it.
## (.new! []) for calling the class's constructor.
## (.resolve! container [value]) for calling the "resolve" method.
```

#####definterface


#####do-to
```
## Call a variety of methods on an object; then return the object.
(do-to vreq
  (HttpServerRequest::setExpectMultipart [true])
  (ReadStream::handler [(object [(Handler Buffer)]
                          []
                          (#override (Handler A) handle [] [(buffer A)] void
                                     (run-io (do IO/Monad
                                               [_ (frplux;write(Buffer::getBytes [] buffer) body)]
                                               (wrap [])))))])

  (ReadStream::endHandler [[(object [(Handler Void)]
                              []
                              (#override (Handler A) handle [] [(_ A)] void
                                         (exec (do Async/Monad
                                                 [#let [_ (run-io (frplux;closebody))]
                                                  response (handler (request$ vreq body))]
                                                 (respond! response vreq))
                                           [])))]]))
```

#####instance?
```
## Checks whether an object is an instance of a particular class.
## Caveat emptor: Can't check for polymorphism, so avoid using parameterized classes.
(instance? String "YOLO")
```

#####jvm-import
```
## Allows importing JVM classes, and using them as types.
## Their methods, fields and enum options can also be imported.
## Also, classes which get imported into a module can also be referred-to with their short names in other macros that require JVM classes.
## Examples:
(jvm-import java.lang.Object
  (new [] [])
  (equals [] [Object] boolean)
  (wait [] [int] #io #try void))

## Special options can also be given for the return values.
## #? means that the values will be returned inside a Maybe type. That way, null becomes #;None.
## #try means that the computation might throw an exception, and the return value will be wrapped by the Error type.
## #io means the computation has side effects, and will be wrapped by the IO type.
## These options must show up in the following order [#io #try #?] (although, each option can be used independently).
(jvm-import java.lang.String
  (new [] [(Array byte)])
  (#static valueOf [] [char] String)
  (#static valueOf #as int-valueOf [] [int] String))

(jvm-import #long (java.util.List e)
  (size [] [] int)
  (get [] [int] e))

(jvm-import (java.util.ArrayList a)
  (toArray [T] [(Array T)] (Array T)))

## #long makes it so the class-type that is generated is of the fully-qualified name.
## In this case, it avoids a clash between the java.util.List type, and Lux's own List type.
(jvm-import java.lang.Character$UnicodeScript
  (#enum ARABIC CYRILLIC LATIN))

## All enum options to be imported must be specified.
(jvm-import #long (lux.concurrency.async.JvmAsync A)
  (resolve [] [A] boolean)
  (poll [] [] A)
  (wasResolved [] [] boolean)
  (waitOn [] [lux.Function] void)
  (#static make [A] [A] (JvmAsync A)))

## It should also be noted, the only types that may show up in method arguments or return values may be Java classes, arrays, primitives, void or type-parameters.
## Lux types, such as Maybe can't be named (otherwise, they'd be confused for Java classes).
## Also, the names of the imported members will look like ClassName::MemberName.
## E.g.:
(Object::new [])

(Object::equals [other-object] my-object)

(java.util.List::size [] my-list)

Character$UnicodeScript::LATIN
```

#####locking
```
## Evaluates body, while holding a lock on a given object.
(locking object-to-be-locked
  (exec (do-something ...)
    (do-something-else ...)
    (finish-the-computation ...)))
```

#####null
```
## Null object pointer.
(null)
```

#####null?
```
## Test for null object pointer.
(null? (null))

## =>
true

(null? "YOLO")

## =>
false
```

#####object
```
## Allows defining anonymous classes.
## The 1st vector corresponds to parent interfaces.
## The 2nd vector corresponds to arguments to the super class constructor.
## An optional super-class can be specified before the 1st vector. If not specified, java.lang.Object will be assumed.
(object [java.lang.Runnable]
  []
  (#override java.lang.Runnable run [] [] void
             (exec (do-something some-input)
               [])))
```

#####program
```
## Defines the entry-point to a program (similar to the "main" function/method in other programming languages).
## Can take a list of all the input parameters to the program, or can destructure them using CLI-option combinators from the lux/cli module.
(program all-args
  (do IO/Monad
    [foo init-program
     bar (do-something all-args)]
    (wrap [])))

(program (name)
  (io (log! (Text:++ "Hello, " name))))

(program ([config config^])
  (do IO/Monad
    [data (init-program config)]
    (do-something data)))
```

#####try
```
## Covers the expression in a try-catch block.
## If it succeeds, you get (#;Right result).
## If it fails, you get (#;Left error+stack-traces-as-text).
(try (risky-computation input))
```

#####with-open
```
## Creates a local-binding with the desired resources, and runs the body (assumed to be in the IO type).
## Afterwards, closes all resources (assumed to be subclasses of java.io.Closeable), and returns the value resulting from running the body.
(with-open [my-res1 (res1-constructor ...)
            my-res2 (res1-constructor ...)]
  (do IO/Monad
    [foo (do-something my-res1)
     bar (do-something-else my-res2)]
    (do-one-last-thing foo bar)))
```



###Values
#####(c2b value)
```
## Type converter.
## From:
java.lang.Character

## To:
java.lang.Byte
```
`(-> (^ java.lang.Character) (^ java.lang.Byte))`

#####(c2i value)
```
## Type converter.
## From:
java.lang.Character

## To:
java.lang.Integer
```
`(-> (^ java.lang.Character) (^ java.lang.Integer))`

#####(c2l value)
```
## Type converter.
## From:
java.lang.Character

## To:
java.lang.Long
```
`(-> (^ java.lang.Character) (^ java.lang.Long))`

#####(c2s value)
```
## Type converter.
## From:
java.lang.Character

## To:
java.lang.Short
```
`(-> (^ java.lang.Character) (^ java.lang.Short))`

#####(d2f value)
```
## Type converter.
## From:
java.lang.Double

## To:
java.lang.Float
```
`(-> (^ java.lang.Double) (^ java.lang.Float))`

#####(d2i value)
```
## Type converter.
## From:
java.lang.Double

## To:
java.lang.Integer
```
`(-> (^ java.lang.Double) (^ java.lang.Integer))`

#####(d2l value)
```
## Type converter.
## From:
java.lang.Double

## To:
java.lang.Long
```
`(-> (^ java.lang.Double) (^ java.lang.Long))`

#####(f2d value)
```
## Type converter.
## From:
java.lang.Float

## To:
java.lang.Double
```
`(-> (^ java.lang.Float) (^ java.lang.Double))`

#####(f2i value)
```
## Type converter.
## From:
java.lang.Float

## To:
java.lang.Integer
```
`(-> (^ java.lang.Float) (^ java.lang.Integer))`

#####(f2l value)
```
## Type converter.
## From:
java.lang.Float

## To:
java.lang.Long
```
`(-> (^ java.lang.Float) (^ java.lang.Long))`

#####(i2b value)
```
## Type converter.
## From:
java.lang.Integer

## To:
java.lang.Byte
```
`(-> (^ java.lang.Integer) (^ java.lang.Byte))`

#####(i2c value)
```
## Type converter.
## From:
java.lang.Integer

## To:
java.lang.Character
```
`(-> (^ java.lang.Integer) (^ java.lang.Character))`

#####(i2d value)
```
## Type converter.
## From:
java.lang.Integer

## To:
java.lang.Double
```
`(-> (^ java.lang.Integer) (^ java.lang.Double))`

#####(i2f value)
```
## Type converter.
## From:
java.lang.Integer

## To:
java.lang.Float
```
`(-> (^ java.lang.Integer) (^ java.lang.Float))`

#####(i2l value)
```
## Type converter.
## From:
java.lang.Integer

## To:
java.lang.Long
```
`(-> (^ java.lang.Integer) (^ java.lang.Long))`

#####(i2s value)
```
## Type converter.
## From:
java.lang.Integer

## To:
java.lang.Short
```
`(-> (^ java.lang.Integer) (^ java.lang.Short))`

#####(l2d value)
```
## Type converter.
## From:
java.lang.Long

## To:
java.lang.Double
```
`(-> (^ java.lang.Long) (^ java.lang.Double))`

#####(l2f value)
```
## Type converter.
## From:
java.lang.Long

## To:
java.lang.Float
```
`(-> (^ java.lang.Long) (^ java.lang.Float))`

#####(l2i value)
```
## Type converter.
## From:
java.lang.Long

## To:
java.lang.Integer
```
`(-> (^ java.lang.Long) (^ java.lang.Integer))`

#####(throwable->text t)
`(-> (^ java.lang.Throwable) lux;Text)`

#lux/math






###Values
#####(->degrees n)
`(-> lux;Real lux;Real)`

#####(->radians n)
`(-> lux;Real lux;Real)`

#####(acos n)
`(-> lux;Real lux;Real)`

#####(asin n)
`(-> lux;Real lux;Real)`

#####(atan n)
`(-> lux;Real lux;Real)`

#####(atan2 x y)
`(-> lux;Real lux;Real lux;Real)`

#####(cbrt n)
`(-> lux;Real lux;Real)`

#####(ceil n)
`(-> lux;Real lux;Real)`

#####(cos n)
`(-> lux;Real lux;Real)`

#####(cosh n)
`(-> lux;Real lux;Real)`

#####e
`lux;Real`

#####(exp n)
`(-> lux;Real lux;Real)`

#####(floor n)
`(-> lux;Real lux;Real)`

#####(gcd a b)
```
Greatest Common Divisor.
```
`(-> lux;Int lux;Int lux;Int)`

#####(lcm x y)
```
Least Common Multiple.
```
`(-> lux;Int lux;Int lux;Int)`

#####(log n)
`(-> lux;Real lux;Real)`

#####pi
`lux;Real`

#####(pow x y)
`(-> lux;Real lux;Real lux;Real)`

#####(round n)
`(-> lux;Real lux;Int)`

#####(sin n)
`(-> lux;Real lux;Real)`

#####(sinh n)
`(-> lux;Real lux;Real)`

#####(sqrt n)
`(-> lux;Real lux;Real)`

#####(tan n)
`(-> lux;Real lux;Real)`

#####(tanh n)
`(-> lux;Real lux;Real)`

#####tau
`lux;Real`

#lux/meta


###Macros
#####with-gensyms
```
## Creates new symbols and offers them to the body expression.
(defsyntax #export (locking lock body)
  (with-gensyms [g!lock g!body g!_]
    (wrap (list (` (let [(~ g!lock) (~ lock)
                         (~ g!_) (lux;_jvm_monitorenter(~ g!lock))
                         (~ g!body) (~ body)
                         (~ g!_) (lux;_jvm_monitorexit(~ g!lock))]
                     (~ g!body)))))))
```

###Structs
#####Lux/Applicative
`(lux/control/applicative;Applicative lux;Lux)`

#####Lux/Functor
`(lux/control/functor;Functor lux;Lux)`

#####Lux/Monad
`(lux/control/monad;Monad lux;Lux)`

###Values
#####(def-meta [v-prefix v-name] state)
`(-> lux;Ident (lux;Lux (lux;Maybe lux;DefData)))`

#####(defs module-name state)
`(-> lux;Text (lux;Lux (lux;List [lux;Text lux;DefData])))`

#####(doc meta)
`(-> lux;DefMeta (lux;Maybe lux;Text))`

#####export?
`(-> lux;DefMeta lux;Bool)`

#####(exported-defs module state)
`(-> lux;Text (lux;Lux (lux;List lux;Text)))`

#####(exports module-name)
`(-> lux;Text (lux;Lux (lux;List [lux;Text lux;DefData])))`

#####(fail msg)
`(All [a] (-> lux;Text (lux;Lux a)))`

#####(find-in-defs name state)
`(-> lux;Ident lux;Compiler (lux;Maybe lux;Type))`

#####(find-in-env name state)
`(-> lux;Text lux;Compiler (lux;Maybe lux;Type))`

#####(find-macro ident)
`(-> lux;Ident (lux;Lux (lux;Maybe lux;Macro)))`

#####(find-module name state)
`(-> lux;Text (lux;Lux lux;Module))`

#####(find-type name)
`(-> lux;Ident (lux;Lux lux;Type))`

#####(find-var-type name)
`(-> lux;Ident (lux;Lux lux;Type))`

#####(flag-set? flag-name meta)
`(-> lux;Ident lux;DefMeta lux;Bool)`

#####(func-args meta)
`(-> lux;DefMeta (lux;List lux;Text))`

#####(gensym prefix state)
`(-> lux;Text (lux;Lux lux;AST))`

#####(get-bool-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;Bool))`

#####(get-char-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;Char))`

#####get-cursor
`(lux;Lux lux;Cursor)`

#####(get-dict-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe (lux;List [lux;Text lux;DefMetaValue])))`

#####get-expected-type
`(lux;Lux lux;Type)`

#####(get-ident-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;Ident))`

#####(get-int-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;Int))`

#####(get-list-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe (lux;List lux;DefMetaValue)))`

#####(get-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;DefMetaValue))`

#####(get-module-name state)
`(lux;Lux lux;Text)`

#####(get-real-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;Real))`

#####(get-text-meta tag meta)
`(-> lux;Ident lux;DefMeta (lux;Maybe lux;Text))`

#####(imported-modules module-name)
`(-> lux;Text (lux;Lux (lux;List lux;Text)))`

#####(macro-expand syntax)
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####(macro-expand-1 token)
`(-> lux;AST (lux;Lux lux;AST))`

#####(macro-expand-all syntax)
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####(macro-expand-once syntax)
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####macro?
`(-> lux;DefMeta lux;Bool)`

#####(module-exists? module state)
`(-> lux;Text (lux;Lux lux;Bool))`

#####(modules state)
`(lux;Lux (lux;List lux;Text))`

#####(normalize ident)
`(-> lux;Ident (lux;Lux lux;Ident))`

#####(run-lux compiler action)
`(All [a] (-> lux;Compiler (lux;Lux a) (lux/data/error;Error [lux;Compiler a])))`

#####sig?
`(-> lux;DefMeta lux;Bool)`

#####struct?
`(-> lux;DefMeta lux;Bool)`

#####(tags-for [module name])
`(-> lux;Ident (lux;Lux (lux;Maybe (lux;List lux;Ident))))`

#####(type-args meta)
`(-> lux;DefMeta (lux;List lux;Text))`

#####type-rec?
`(-> lux;DefMeta lux;Bool)`

#####type?
`(-> lux;DefMeta lux;Bool)`

#lux/meta/ast




###Structs
#####AST/Eq
`(lux/control/eq;Eq lux;AST)`

#####AST/Show
`(lux/control/show;Show lux;AST)`

###Values
#####(bool x)
`(-> lux;Bool lux;AST)`

#####(char x)
`(-> lux;Char lux;AST)`

#####(form x)
`(-> (lux;List lux;AST) lux;AST)`

#####(int x)
`(-> lux;Int lux;AST)`

#####(local-symbol name)
`(-> lux;Text lux;AST)`

#####(local-tag name)
`(-> lux;Text lux;AST)`

#####(real x)
`(-> lux;Real lux;AST)`

#####(record x)
`(-> (lux;List [lux;AST lux;AST]) lux;AST)`

#####(symbol x)
`(-> lux;Ident lux;AST)`

#####(tag x)
`(-> lux;Ident lux;AST)`

#####(text x)
`(-> lux;Text lux;AST)`

#####(tuple x)
`(-> (lux;List lux;AST) lux;AST)`

#lux/meta/syntax
###Types
#####Parser
```
(deftype (Parser a)
  (-> (lux;List lux;AST) (lux/data/error;Error [(lux;List lux;AST) a])))
```

###Macros
#####defsyntax
```
## A more advanced way to define macros than defmacro.
## The inputs to the macro can be parsed in complex ways through the use of syntax parsers.
## The macro body is also (implicitly) run in the Lux/Monad, to save some typing.
## Also, the compiler state can be accessed through the *state* binding.
(defsyntax #export (object [#let [imports (class-imports *state*)]]
                     [#let [class-vars (list)]]
                     [super (?^ (super-class-decl^ imports class-vars))]
                     [interfaces (tuple^ (*^ (super-class-decl^ imports class-vars)))]
                     [constructor-args (constructor-args^ imports class-vars)]
                     [methods (*^ (overriden-method-def^ imports))])
  (let [=super (super-class-decl$ (? object-super-class super))
        =interfaces (map super-class-decl$ interfaces)
        =methods (map (method-def$ id) methods)]
    (wrap (list (` (lux;_jvm_anon-class(~ =super)
                                     [(~@ =interfaces)]
                                     [(~@ (map constructor-arg$ constructor-args))]
                                     [(~@ =methods)]))))))
```

###Structs
#####Parser/Applicative
`(lux/control/applicative;Applicative Parser)`

#####Parser/Functor
`(lux/control/functor;Functor Parser)`

#####Parser/Monad
`(lux/control/monad;Monad Parser)`

###Values
#####(&^ p1 p2)
`(All [a b] (-> (Parser a) (Parser b) (Parser [a b])))`

#####(*^ p tokens)
`(All [a] (-> (Parser a) (Parser (lux;List a))))`

#####(+^ p)
`(All [a] (-> (Parser a) (Parser (lux;List a))))`

#####(?^ p tokens)
`(All [a] (-> (Parser a) (Parser (lux;Maybe a))))`

#####(assert^ v message tokens)
`(-> lux;Bool lux;Text (Parser lux;Unit))`

#####(between^ from to p)
`(All [a] (-> lux;Int lux;Int (Parser a) (Parser (lux;List a))))`

#####(bool!^ v tokens)
`(-> lux;Bool (Parser lux;Unit))`

#####(bool?^ v tokens)
`(-> lux;Bool (Parser lux;Bool))`

#####(bool^ tokens)
`(Parser lux;Bool)`

#####(char!^ v tokens)
`(-> lux;Char (Parser lux;Unit))`

#####(char?^ v tokens)
`(-> lux;Char (Parser lux;Bool))`

#####(char^ tokens)
`(Parser lux;Char)`

#####(count^ n p)
`(All [a] (-> lux;Int (Parser a) (Parser (lux;List a))))`

#####(end^ tokens)
`(Parser lux;Unit)`

#####(form^ p tokens)
`(All [a] (-> (Parser a) (Parser a)))`

#####(id^ tokens)
`(Parser lux;AST)`

#####(int!^ v tokens)
`(-> lux;Int (Parser lux;Unit))`

#####(int?^ v tokens)
`(-> lux;Int (Parser lux;Bool))`

#####(int^ tokens)
`(Parser lux;Int)`

#####(local-symbol^ tokens)
`(Parser lux;Text)`

#####(local-tag^ tokens)
`(Parser lux;Text)`

#####nat^
`(Parser lux;Int)`

#####(not^ p)
`(All [a] (-> (Parser a) (Parser lux;Unit)))`

#####(real!^ v tokens)
`(-> lux;Real (Parser lux;Unit))`

#####(real?^ v tokens)
`(-> lux;Real (Parser lux;Bool))`

#####(real^ tokens)
`(Parser lux;Real)`

#####(record^ p tokens)
`(All [a] (-> (Parser a) (Parser a)))`

#####(run-parser p tokens)
`(All [a] (-> (Parser a) (lux;List lux;AST) (lux/data/error;Error [(lux;List lux;AST) a])))`

#####(sep-by^ sep p)
`(All [a b] (-> (Parser b) (Parser a) (Parser (lux;List a))))`

#####(symbol!^ v tokens)
`(-> lux;Ident (Parser lux;Unit))`

#####(symbol?^ v tokens)
`(-> lux;Ident (Parser lux;Bool))`

#####(symbol^ tokens)
`(Parser lux;Ident)`

#####(tag!^ v tokens)
`(-> lux;Ident (Parser lux;Unit))`

#####(tag?^ v tokens)
`(-> lux;Ident (Parser lux;Bool))`

#####(tag^ tokens)
`(Parser lux;Ident)`

#####(text!^ v tokens)
`(-> lux;Text (Parser lux;Unit))`

#####(text?^ v tokens)
`(-> lux;Text (Parser lux;Bool))`

#####(text^ tokens)
`(Parser lux;Text)`

#####(tuple^ p tokens)
`(All [a] (-> (Parser a) (Parser a)))`

#####(up-to^ n p)
`(All [a] (-> lux;Int (Parser a) (Parser (lux;List a))))`

#####(|^ p1 p2 tokens)
`(All [a b] (-> (Parser a) (Parser b) (Parser (| a b))))`

#####(||^ ps tokens)
`(All [a] (-> (lux;List (Parser a)) (Parser a)))`

#lux/meta/type




###Structs
#####Type/Eq
`(lux/control/eq;Eq lux;Type)`

#####Type/Show
`(lux/control/show;Show lux;Type)`

###Values
#####(apply-type type-fun param)
`(-> lux;Type lux;Type (lux;Maybe lux;Type))`