#lux
###Types
#####Bool
```
(^ java.lang.Boolean)
```

#####Int
```
(^ java.lang.Long)
```

#####Real
```
(^ java.lang.Double)
```

#####Char
```
(^ java.lang.Character)
```

#####Text
```
(^ java.lang.String)
```

#####Void
```
Void
```

#####Unit
```
Unit
```

#####Ident
```
(, Text Text)
```

#####List
```
(All [a]
  (| #Nil     (#Cons (, a (List a)))))
```

#####Maybe
```
(All [a]
  (| #None     (#Some a)))
```

#####Type
```
((All [a]
  (| (#DataT (, Text (List (Type a))))     #VoidT     #UnitT     (#SumT (, (Type a) (Type a)))     (#ProdT (, (Type a) (Type a)))     (#LambdaT (, (Type a) (Type a)))     (#BoundT Int)     (#VarT Int)     (#ExT Int)     (#UnivQ (, (List (Type a)) (Type a)))     (#ExQ (, (List (Type a)) (Type a)))     (#AppT (, (Type a) (Type a)))     (#NamedT (, Ident (Type a))))) Void)
```

#####DefMetaValue
```
((All [a]
  (| (#BoolM Bool)     (#IntM Int)     (#RealM Real)     (#CharM Char)     (#TextM Text)     (#IdentM Ident)     (#ListM (List (DefMetaValue a)))     (#DictM (List (, Text (DefMetaValue a)))))) Void)
```

#####DefMeta
```
(List (, Ident DefMetaValue))
```

#####DefData
```
(, Type DefMeta Unit)
```

#####Bindings
```
(All [a b]
  (& #counter Int     #mappings (List (, a b))))
```

#####Cursor
```
(& #module Text
   #line Int
   #column Int)
```

#####Meta
```
(All [a b]
  (& #meta a     #datum b))
```

#####Analysis
```
(Meta (, Type Cursor) Void)
```

#####Env
```
(& #name Text
   #inner-closures Int
   #locals (Bindings Text Analysis)
   #closure (Bindings Text Analysis))
```

#####AST'
```
(All [a]
  (| (#BoolS Bool)     (#IntS Int)     (#RealS Real)     (#CharS Char)     (#TextS Text)     (#SymbolS Ident)     (#TagS Ident)     (#FormS (List (a (AST' a))))     (#TupleS (List (a (AST' a))))     (#RecordS (List (, (a (AST' a)) (a (AST' a)))))))
```

#####AST
```
(Meta Cursor (AST' (Meta Cursor)))
```

#####Either
```
(All [a b]
  (| (#Left a)     (#Right b)))
```

#####Source
```
(List (Meta Cursor Text))
```

#####Module
```
(& #module-aliases (List (, Text Text))
   #defs (List (, Text DefData))
   #imports (List Text)
   #tags (List (, Text Int (List Ident) Type))
   #types (List (, Text (List Ident) Type)))
```

#####Compiler
```
((All [a]
  (& #source Source     #cursor Cursor     #modules (List (, Text Module))     #envs (List Env)     #type-vars (Bindings Int Type)     #expected Type     #seed Int     #eval? Bool     #host Void)) Void)
```

#####Macro
```
(-> (List AST) ((All [a b] (-> a (Either Text (, a b)))) Compiler (List AST)))
```

###Macros
#####comment
Example(s):
```
(comment 1 2 3 4) ## Same as not writing anything...
```

#####All
Example(s):
```
## Universal quantification.
(All List [a]
     (| #Nil
        (#Cons (, a (List a)))))

## It must be explicit, unlike in Haskell.
## Rank-n types will be possible as well as existential types
(All [a]
  (-> a a))
```

#####->
Example(s):
```
## Function types
(-> Int Int Int) ## This is the type of a function that takes 2 Ints and returns an Int
```

#####,
Example(s):
```
## Tuples
(, Text Int Bool)

(,) ## The empty tuple, aka Unit
```

#####||
Example(s):
```
## Variants
(|| Text Int Bool)

(||) ## The empty tuple, aka Void
```

#####$
Example(s):
```
## Application of binary functions over variadic arguments.
($ text:++ "Hello, " name ".\nHow are you?")
=>
(text:++ "Hello, " (text:++ name ".\nHow are you?"))
```

#####if
Example(s):
```
(if true
  "Oh, yeah!"
  "Aw hell naw!")
```

#####^
Example(s):
```
## Macro to treat classes as types
(^ java.lang.Object)
```

#####`
Example(s):
```
## Quasi-quotation as a macro. Unquote (~) and unquote-splice (~@) must also be used as forms
(` (def (~ name)
     (lambda [(~@ args)]
       (~ body))))
```

#####'
Example(s):
```
## Quotation as a macro
(' "YOLO")
```

#####|>
Example(s):
```
## Piping
(|> elems (map ->text) (interpose " ") (fold text:++ ""))
=>
(fold text:++ ""
      (interpose " "
                 (map ->text elems)))
```

#####do-template
Example(s):
```
(do-template [<name> <diff>]
  [(def #export <name>
     (-> Int Int)
     (i+ <diff>))]

  [inc 1]
  [dec -1])
```

#####@type
Example(s):
```
## Takes a type expression and returns it's representation as data-structure.
(@type (All [a] (Maybe (List a))))
```

#####:
Example(s):
```
## The type-annotation macro
(: (List Int) (list 1 2 3))
```

#####:!
Example(s):
```
## The type-coercion macro
(:! Dinosaur (list 1 2 3))
```

#####exec
Example(s):
```
## Sequential execution of expressions (great for side-effects).
(exec
  (println! "#1")
  (println! "#2")
  (println! "#3")
  "YOLO")
```

#####case
Example(s):
```
## The pattern-matching macro.
## Allows the usage of macros within the patterns to provide custom syntax.
(case (: (List Int) (@list 1 2 3))
  (#Cons x (#Cons y (#Cons z #Nil)))
  (#Some ($ int:* x y z))

  _
  #None)
```

#####\
Example(s):
```
## It's a special macro meant to be used with case
(case (: (List Int) (@list 1 2 3))
  (\ (@list x y z))
  (#Some ($ int:* x y z))

  _
  #None)
```

#####\or
Example(s):
```
## It's a special macro meant to be used with case
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

#####let
Example(s):
```
(let [x (foo bar)
      y (baz quux)]
  (op x y))
```

#####lambda
Example(s):
```
(def const
  (lambda [x y] x))

(def const
  (lambda const [x y] x))
```

#####def
Example(s):
```
## Macro for definining global constants/functions.
(def (rejoin-pair pair)
  (-> (, Syntax Syntax) (List Syntax))
  (let [[left right] pair]
    (list left right)))
```

#####defmacro
Example(s):
```
(defmacro #export (export tokens)
  (return (map (: (-> AST AST) (lambda [token] (` (;_lux_export (~ token))))) tokens)))
```

#####defsig
Example(s):
```
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
```

#####struct
Example(s):
```
## Not mean to be used directly. Prefer defstruct
```

#####defstruct
Example(s):
```
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
```

#####and
Example(s):
```
(and true false true) ## => false
```

#####or
Example(s):
```
(or true false true) ## => true
```

#####deftype
Example(s):
```
## The type-definition macro
(deftype (List a)
  (| #Nil
     (#Cons (, a (List a)))))
```

#####using
Example(s):
```
## Opens up a structure and provides all the definitions as local variables.
(using Int/Ord
  (< 5 10))
```

#####\open
Example(s):
```
(def #export (range (\open) from to)
  (All [a] (-> (Enum a) a a (List a)))
  (range' <= succ from to))
```

#####cond
Example(s):
```
(cond (even? num) "even"
      (odd? num) "odd"
      ## else-branch
      "???")
```

#####get@
Example(s):
```
(get@ #field my-record)
```

#####open
Example(s):
```
(open List/Monoid "list:++")
(open Int/Number)
```

#####import
Example(s):
```
(;import lux
         (lux (control (monad #as M #refer #all))
              (data (text #open ("text:" Text/Monoid))
                    (list #open ("list:" List/Monad))
                    maybe
                    (ident #open ("ident:" Ident/Show)))
              (meta ast
                    lux))
         (.. (type #open ("" Type/Eq Type/Show))))
```

#####::
Example(s):
```
(:: Int/Show (show 123))
```

#####set@
Example(s):
```
(set@ #name "Lux" lang)
```

#####update@
Example(s):
```
(update@ #age inc person)
```

#####\template
Example(s):
```
(def #export (beta-reduce env type)
  (-> (List Type) Type Type)
  (case type
    (\template [<tag>]
     [(<tag> members)
      (<tag> (list:map (beta-reduce env) members))])
    ([#;VariantT]
     [#;TupleT])

    (\template [<tag>]
     [(<tag> left right)
      (<tag> (beta-reduce env left) (beta-reduce env right))])
    ([#;LambdaT]
     [#;AppT])

    (\template [<tag>]
     [(<tag> env def)
      (case env
        #;Nil
        (<tag> env def)

        _
        type)])
    ([#;UnivQ]
     [#;ExQ])

    (#;BoundT idx)
    (? type (@ idx env))

    (#;NamedT name type)
    (beta-reduce env type)

    _
    type
    ))
```

#####loop
Example(s):
```
(loop [count 0
       x init]
  (if (> count 10)
    x
    (recur (inc count) (f x))))
```

#####\slots
Example(s):
```
(let [(\slots [#foo #bar #baz]) quux]
  (f foo bar baz))
```



###Values
#####splice-helper
`(-> (List AST) (List AST) (List AST))`

#####Lux
`Type`

#####.
`(All [a b c] (-> (-> b c) (-> a b) a c))`

#####not
`(-> Bool Bool)`

#####id
`(All [a] (-> a a))`

#####inc
`(-> Int Int)`

#####dec
`(-> Int Int)`

___

#lux/control/monoid
###Types
#####Monoid
```
(All [a]
  (& #unit a     #++ (-> a a a)))
```





###Values
#####monoid$
`(All [a] (-> a (-> a a a) (Monoid a)))`

___

#lux/control/functor
###Types
#####Functor
```
(All [a b c]
  (-> (-> b c) (a b) (a c)))
```







___

#lux/control/monad
###Types
#####Monad
```
(All [a]
  (& #_functor (lux/control/functor;Functor a)     #wrap (All [b] (-> b (a b)))     #join (All [b] (-> (a (a b)) (a b)))))
```

###Macros
#####do
Example(s):
```
(do Maybe/Monad
  [y (f1 x)
   z (f2 z)]
  (wrap (f3 z)))
```



###Values
#####bind
`(All [a b c] (-> (Monad a) (-> b (a c)) (a b) (a c)))`

#####seq%
`(All [a b] (-> (Monad a) (lux;List (a b)) (a (lux;List b))))`

#####map%
`(All [a b c] (-> (Monad a) (-> b (a c)) (lux;List b) (a (lux;List c))))`

#####foldL%
`(All [a b c] (-> (Monad a) (-> b c (a b)) b (lux;List c) (a b)))`

___

#lux/codata/function






###Values
#####const
`(All [a b] (-> a b a))`

#####flip
`(All [a b c] (-> (-> a b c) b a c))`

#####Comp/Monoid
`(All [a] (lux/control/monoid;Monoid (-> a a)))`

___

#lux/data/tuple






###Values
#####first
`(All [a b] (-> (, a b) a))`

#####second
`(All [a b] (-> (, a b) b))`

#####curry
`(All [a b c] (-> (-> (, a b) c) a b c))`

#####uncurry
`(All [a b c] (-> (-> a b c) (, a b) c))`

#####swap
`(All [a b] (-> (, a b) (, b a)))`

___

#lux/data/maybe






###Values
#####Maybe/Monoid
`(All [a] (lux/control/monoid;Monoid (lux;Maybe a)))`

#####Maybe/Functor
`(lux/control/functor;Functor lux;Maybe)`

#####Maybe/Monad
`(lux/control/monad;Monad lux;Maybe)`

#####?
`(All [a] (-> a (lux;Maybe a) a))`

___

#lux/control/read
###Types
#####Read
```
(All [a]
  (-> lux;Text (lux;Maybe a)))
```







___

#lux/control/show
###Types
#####Show
```
(All [a]
  (-> a lux;Text))
```







___

#lux/control/bounded
###Types
#####Bounded
```
(All [a]
  (& #top a     #bottom a))
```







___

#lux/control/eq
###Types
#####Eq
```
(All [a]
  (-> a a lux;Bool))
```







___

#lux/control/ord
###Types
#####Ord
```
(All [a]
  (& #_eq (lux/control/eq;Eq a)     #< (-> a a lux;Bool)     #<= (-> a a lux;Bool)     #> (-> a a lux;Bool)     #>= (-> a a lux;Bool)))
```





###Values
#####ord$
`(All [a] (-> (lux/control/eq;Eq a) (-> a a lux;Bool) (-> a a lux;Bool) (Ord a)))`

#####max
`(All [a] (-> (Ord a) a a a))`

#####min
`(All [a] (-> (Ord a) a a a))`

___

#lux/control/enum
###Types
#####Enum
```
(All [a]
  (& #_ord (lux/control/ord;Ord a)     #succ (-> a a)     #pred (-> a a)))
```





###Values
#####range
`(All [a] (-> (Enum a) a a (lux;List a)))`

___

#lux/control/number
###Types
#####Number
```
(All [a]
  (& #+ (-> a a a)     #- (-> a a a)     #* (-> a a a)     #/ (-> a a a)     #% (-> a a a)     #negate (-> a a)     #signum (-> a a)     #abs (-> a a)     #from-int (-> lux;Int a)))
```







___

#lux/data/number






###Values
#####Int/Number
`(lux/control/number;Number lux;Int)`

#####Real/Number
`(lux/control/number;Number lux;Real)`

#####Int/Eq
`(lux/control/eq;Eq lux;Int)`

#####Real/Eq
`(lux/control/eq;Eq lux;Real)`

#####Int/Ord
`(lux/control/ord;Ord lux;Int)`

#####Real/Ord
`(lux/control/ord;Ord lux;Real)`

#####Int/Enum
`(lux/control/enum;Enum lux;Int)`

#####Real/Enum
`(lux/control/enum;Enum lux;Real)`

#####Int/Bounded
`(lux/control/bounded;Bounded lux;Int)`

#####Real/Bounded
`(lux/control/bounded;Bounded lux;Real)`

#####IntAdd/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMul/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMax/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMin/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####RealAdd/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMul/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMax/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMin/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####Int/Show
`(lux/control/show;Show lux;Int)`

#####Real/Show
`(lux/control/show;Show lux;Real)`

#####Int/Read
`(lux/control/read;Read lux;Int)`

#####Real/Read
`(lux/control/read;Read lux;Real)`

#####->int
`(-> lux;Real lux;Int)`

#####->real
`(-> lux;Int lux;Real)`

___

#lux/data/text


###Macros
#####<>
Example(s):
```
(let [name "John Doe"]
  (<> "Welcome, #{name}"))
```



###Values
#####size
`(-> lux;Text lux;Int)`

#####@
`(-> lux;Int lux;Text (lux;Maybe lux;Char))`

#####contains?
`(-> lux;Text lux;Text lux;Bool)`

#####lower-case
`(-> lux;Text lux;Text)`

#####upper-case
`(-> lux;Text lux;Text)`

#####trim
`(-> lux;Text lux;Text)`

#####sub
`(-> lux;Int lux;Int lux;Text (lux;Maybe lux;Text))`

#####sub'
`(-> lux;Int lux;Text (lux;Maybe lux;Text))`

#####replace
`(-> lux;Text lux;Text lux;Text lux;Text)`

#####index-of'
`(-> lux;Text lux;Int lux;Text (lux;Maybe lux;Int))`

#####index-of
`(-> lux;Text lux;Text (lux;Maybe lux;Int))`

#####last-index-of'
`(-> lux;Text lux;Int lux;Text (lux;Maybe lux;Int))`

#####last-index-of
`(-> lux;Text lux;Text (lux;Maybe lux;Int))`

#####starts-with?
`(-> lux;Text lux;Text lux;Bool)`

#####ends-with?
`(-> lux;Text lux;Text lux;Bool)`

#####split
`(-> lux;Int lux;Text (lux;Maybe (, lux;Text lux;Text)))`

#####split-with
`(-> lux;Text lux;Text (lux;Maybe (, lux;Text lux;Text)))`

#####split-all-with
`(-> lux;Text lux;Text (lux;List lux;Text))`

#####split-lines
`(-> lux;Text (lux;List lux;Text))`

#####Text/Eq
`(lux/control/eq;Eq lux;Text)`

#####Text/Ord
`(lux/control/ord;Ord lux;Text)`

#####Text/Show
`(lux/control/show;Show lux;Text)`

#####Text/Monoid
`(lux/control/monoid;Monoid lux;Text)`

___

#lux/data/bool






###Values
#####Bool/Eq
`(lux/control/eq;Eq lux;Bool)`

#####Or/Monoid
`(lux/control/monoid;Monoid lux;Bool)`

#####And/Monoid
`(lux/control/monoid;Monoid lux;Bool)`

#####Bool/Show
`(lux/control/show;Show lux;Bool)`

#####Bool/Read
`(lux/control/read;Read lux;Bool)`

#####comp
`(All [a] (-> (-> a lux;Bool) a lux;Bool))`

___

#lux/control/fold
###Types
#####Fold
```
(All [a]
  (& #foldL (All [b c] (-> (-> b c b) b (a c) b))     #foldR (All [b c] (-> (-> c b b) b (a c) b))))
```





###Values
#####fold
`(All [a b] (-> (lux/control/monoid;Monoid b) (Fold a) (a b) b))`

#####size
`(All [a b] (-> (Fold a) (a b) lux;Int))`

#####member?
`(All [a b] (-> (lux/control/eq;Eq b) (Fold a) b (a b) lux;Bool))`

#####empty?
`(All [a b] (-> (Fold a) (a b) lux;Bool))`

___

#lux/data/list


###Macros
#####list
Example(s):
```
(list 1 2 3)
```

#####list&
Example(s):
```
(list& 1 2 3 (@list 4 5 6))
```

#####zip
Example(s):
```
((zip 3) xs ys zs)
```



###Values
#####List/Fold
`(lux/control/fold;Fold lux;List)`

#####reverse
`(All [a] (-> (lux;List a) (lux;List a)))`

#####filter
`(All [a] (-> (-> a lux;Bool) (lux;List a) (lux;List a)))`

#####partition
`(All [a] (-> (-> a lux;Bool) (lux;List a) (, (lux;List a) (lux;List a))))`

#####as-pairs
`(All [a] (-> (lux;List a) (lux;List (, a a))))`

#####take
`(All [a] (-> lux;Int (lux;List a) (lux;List a)))`

#####drop
`(All [a] (-> lux;Int (lux;List a) (lux;List a)))`

#####take-while
`(All [a] (-> (-> a lux;Bool) (lux;List a) (lux;List a)))`

#####drop-while
`(All [a] (-> (-> a lux;Bool) (lux;List a) (lux;List a)))`

#####split
`(All [a] (-> lux;Int (lux;List a) (, (lux;List a) (lux;List a))))`

#####split-with
`(All [a] (-> (-> a lux;Bool) (lux;List a) (, (lux;List a) (lux;List a))))`

#####repeat
`(All [a] (-> lux;Int a (lux;List a)))`

#####iterate
`(All [a] (-> (-> a (lux;Maybe a)) a (lux;List a)))`

#####some
`(All [a b] (-> (-> a (lux;Maybe b)) (lux;List a) (lux;Maybe b)))`

#####interpose
`(All [a] (-> a (lux;List a) (lux;List a)))`

#####size
`(All [a] (-> (lux;List a) lux;Int))`

#####every?
`(All [a] (-> (-> a lux;Bool) (lux;List a) lux;Bool))`

#####any?
`(All [a] (-> (-> a lux;Bool) (lux;List a) lux;Bool))`

#####@
`(All [a] (-> lux;Int (lux;List a) (lux;Maybe a)))`

#####List/Eq
`(All [a] (-> (lux/control/eq;Eq a) (lux/control/eq;Eq (lux;List a))))`

#####List/Monoid
`(All [a] (lux/control/monoid;Monoid (lux;List a)))`

#####List/Functor
`(lux/control/functor;Functor lux;List)`

#####List/Monad
`(lux/control/monad;Monad lux;List)`

#####sort
`(All [a] (-> (lux/control/ord;Ord a) (lux;List a) (lux;List a)))`

#####range
`(-> lux;Int lux;Int (lux;List lux;Int))`

#####zip2
`(All [a b] (-> (lux;List a) (lux;List b) (lux;List (, a b))))`

#####zip3
`(All [a b c] (-> (lux;List a) (lux;List b) (lux;List c) (lux;List (, a b c))))`

#####empty?
`(All [a] (-> (lux;List a) lux;Bool))`

#####member?
`(All [a] (-> (lux/control/eq;Eq a) a (lux;List a) lux;Bool))`

___

#lux/control/comonad
###Types
#####CoMonad
```
(All [a]
  (& #_functor (lux/control/functor;Functor a)     #unwrap (All [b] (-> (a b) b))     #split (All [b] (-> (a b) (a (a b))))))
```

###Macros
#####be
Example(s):
```
(be Stream/CoMonad
  [ys (f1 xs)
   zs (f2 ys)]
  (head (tail zs)))
```



###Values
#####extend
`(All [a b c] (-> (CoMonad a) (-> (a b) c) (a b) (a c)))`

___

#lux/control/hash
###Types
#####Hash
```
(All [a]
  (-> a lux;Int))
```







___

#lux/data/char






###Values
#####Char/Eq
`(lux/control/eq;Eq lux;Char)`

#####Char/Show
`(lux/control/show;Show lux;Char)`

#####->text
`(-> lux;Char lux;Text)`

___

#lux/data/either






###Values
#####either
`(All [a b c] (-> (-> a c) (-> b c) (lux;Either a b) c))`

#####lefts
`(All [a b] (-> (lux;List (lux;Either a b)) (lux;List a)))`

#####rights
`(All [a b] (-> (lux;List (lux;Either a b)) (lux;List b)))`

#####partition
`(All [a b] (-> (lux;List (lux;Either a b)) (, (lux;List a) (lux;List b))))`

#####Either/Functor
`(All [a] (lux/control/functor;Functor (lux;Either a)))`

#####Either/Monad
`(All [a] (lux/control/monad;Monad (lux;Either a)))`

___

#lux/data/id
###Types
#####Id
```
(All [a] a)
```





###Values
#####Id/Functor
`(lux/control/functor;Functor Id)`

#####Id/Monad
`(lux/control/monad;Monad Id)`

#####Id/CoMonad
`(lux/control/comonad;CoMonad Id)`

___

#lux/data/ident






###Values
#####module
`(-> lux;Ident lux;Text)`

#####name
`(-> lux;Ident lux;Text)`

#####Ident/Eq
`(lux/control/eq;Eq lux;Ident)`

#####Ident/Show
`(lux/control/show;Show lux;Ident)`

___

#lux/data/writer
###Types
#####Writer
```
(All [a b] (, a b))
```





###Values
#####Writer/Functor
`(All [a] (lux/control/functor;Functor (Writer a)))`

#####Writer/Monad
`(All [a] (-> (lux/control/monoid;Monoid a) (lux/control/monad;Monad (Writer a))))`

___

#lux/meta/ast






###Values
#####bool
`(-> lux;Bool lux;AST)`

#####int
`(-> lux;Int lux;AST)`

#####real
`(-> lux;Real lux;AST)`

#####char
`(-> lux;Char lux;AST)`

#####text
`(-> lux;Text lux;AST)`

#####symbol
`(-> lux;Ident lux;AST)`

#####tag
`(-> lux;Ident lux;AST)`

#####form
`(-> (lux;List lux;AST) lux;AST)`

#####tuple
`(-> (lux;List lux;AST) lux;AST)`

#####record
`(-> (lux;List (, lux;AST lux;AST)) lux;AST)`

#####AST/Show
`(lux/control/show;Show lux;AST)`

#####AST/Eq
`(lux/control/eq;Eq lux;AST)`

___

#lux/codata/lazy
###Types
#####Lazy
```
(All [a b] (-> (-> a b) b))
```

###Macros
#####...
Example(s):
```
(def my-thunk (... (+ 1 2)))
```



###Values
#####!
`(All [a] (-> (Lazy a) a))`

#####call/cc
`(All [a b c] (Lazy (-> a (Lazy b c)) (Lazy a c)))`

#####run-lazy
`(All [a b] (-> (Lazy a b) (-> a b) b))`

#####Lazy/Functor
`(lux/control/functor;Functor Lazy)`

#####Lazy/Monad
`(lux/control/monad;Monad Lazy)`

___

#lux/meta/lux






###Values
#####Lux/Functor
`(lux/control/functor;Functor lux;Lux)`

#####Lux/Monad
`(lux/control/monad;Monad lux;Lux)`

#####get-module-name
`(lux;Lux lux;Text)`

#####find-macro
`(-> lux;Ident (lux;Lux (lux;Maybe lux;Macro)))`

#####normalize
`(-> lux;Ident (lux;Lux lux;Ident))`

#####macro-expand-once
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####macro-expand
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####macro-expand-all
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####gensym
`(-> lux;Text (lux;Lux lux;AST))`

#####fail
`(All [a] (-> lux;Text (lux;Lux a)))`

#####macro-expand-1
`(-> lux;AST (lux;Lux lux;AST))`

#####module-exists?
`(-> lux;Text (lux;Lux lux;Bool))`

#####exporteds-defs
`(-> lux;Text (lux;Lux (lux;List lux;Text)))`

#####find-in-env
`(-> lux;Text lux;Compiler (lux;Maybe lux;Type))`

#####find-in-defs
`(-> lux;Ident lux;Compiler (lux;Maybe lux;Type))`

#####find-var-type
`(-> lux;Ident (lux;Lux lux;Type))`

#####find-type
`(-> lux;Ident (lux;Lux lux;Type))`

#####defs
`(-> lux;Text (lux;Lux (lux;List (, lux;Text lux;DefData))))`

#####exports
`(-> lux;Text (lux;Lux (lux;List (, lux;Text lux;DefData))))`

#####modules
`(lux;Lux (lux;List lux;Text))`

#####find-module
`(-> lux;Text (lux;Lux lux;Module))`

#####tags-for
`(-> lux;Ident (lux;Lux (lux;Maybe (lux;List lux;Ident))))`

___

#lux/meta/syntax
###Types
#####Parser
```
(All [a] (-> (lux;List lux;AST) (lux;Either lux;Text (, (lux;List lux;AST) a))))
```

###Macros
#####defsyntax
Example(s):
```
(defsyntax #export (object [super local-symbol^] [interfaces (tuple^ (*^ local-symbol^))]
                           [methods (*^ method-def^)])
  (emit (@list (` (;_jvm_anon-class (~ (text$ super))
                                    [(~@ (map text$ interfaces))]
                                    [(~@ (map gen-method-def methods))])))))
```



###Values
#####Parser/Functor
`(lux/control/functor;Functor Parser)`

#####Parser/Monad
`(lux/control/monad;Monad Parser)`

#####id^
`(Parser lux;AST)`

#####bool^
`(Parser lux;Bool)`

#####bool?^
`(-> lux;Bool (Parser lux;Bool))`

#####bool!^
`(-> lux;Bool (Parser Unit))`

#####int^
`(Parser lux;Int)`

#####int?^
`(-> lux;Int (Parser lux;Bool))`

#####int!^
`(-> lux;Int (Parser Unit))`

#####real^
`(Parser lux;Real)`

#####real?^
`(-> lux;Real (Parser lux;Bool))`

#####real!^
`(-> lux;Real (Parser Unit))`

#####char^
`(Parser lux;Char)`

#####char?^
`(-> lux;Char (Parser lux;Bool))`

#####char!^
`(-> lux;Char (Parser Unit))`

#####text^
`(Parser lux;Text)`

#####text?^
`(-> lux;Text (Parser lux;Bool))`

#####text!^
`(-> lux;Text (Parser Unit))`

#####symbol^
`(Parser lux;Ident)`

#####symbol?^
`(-> lux;Ident (Parser lux;Bool))`

#####symbol!^
`(-> lux;Ident (Parser Unit))`

#####tag^
`(Parser lux;Ident)`

#####tag?^
`(-> lux;Ident (Parser lux;Bool))`

#####tag!^
`(-> lux;Ident (Parser Unit))`

#####assert
`(-> lux;Bool lux;Text (Parser Unit))`

#####nat^
`(Parser lux;Int)`

#####local-symbol^
`(Parser lux;Text)`

#####local-tag^
`(Parser lux;Text)`

#####form^
`(All [a] (-> (Parser a) (Parser a)))`

#####tuple^
`(All [a] (-> (Parser a) (Parser a)))`

#####record^
`(All [a] (-> (Parser a) (Parser a)))`

#####?^
`(All [a] (-> (Parser a) (Parser (lux;Maybe a))))`

#####*^
`(All [a] (-> (Parser a) (Parser (lux;List a))))`

#####+^
`(All [a] (-> (Parser a) (Parser (lux;List a))))`

#####&^
`(All [a b] (-> (Parser a) (Parser b) (Parser (, a b))))`

#####|^
`(All [a b] (-> (Parser a) (Parser b) (Parser (lux;Either a b))))`

#####||^
`(All [a] (-> (lux;List (Parser a)) (Parser a)))`

#####end^
`(Parser Unit)`

#####size-n^
`(All [a] (-> lux;Int (Parser (lux;List a)) (Parser (lux;List a))))`

#####join-error-messages
`(-> lux;Text lux;Text lux;Text)`

___

#lux/codata/stream
###Types
#####Stream
```
(All [a] (lux/codata/lazy;Lazy (, a (Stream a))))
```

###Macros
#####\stream&
Example(s):
```
(let [(\stream& x y z _others) (some-stream-func 1 2 3)]
  (func x y z))
```



###Values
#####iterate
`(All [a] (-> (-> a a) a (Stream a)))`

#####repeat
`(All [a] (-> a (Stream a)))`

#####cycle
`(All [a] (-> (lux;List a) (lux;Maybe (Stream a))))`

#####head
`(All [a] (-> (Stream a) a))`

#####tail
`(All [a] (-> (Stream a) (Stream a)))`

#####@
`(All [a] (-> lux;Int (Stream a) a))`

#####take-while
`(All [a] (-> (-> a lux;Bool) (Stream a) (lux;List a)))`

#####drop-while
`(All [a] (-> (-> a lux;Bool) (Stream a) (Stream a)))`

#####split-with
`(All [a] (-> (-> a lux;Bool) (Stream a) (, (lux;List a) (Stream a))))`

#####take
`(All [a] (-> lux;Int (Stream a) (lux;List a)))`

#####drop
`(All [a] (-> lux;Int (Stream a) (Stream a)))`

#####split
`(All [a] (-> lux;Int (Stream a) (, (lux;List a) (Stream a))))`

#####unfold
`(All [a b] (-> (-> a (, a b)) a (Stream b)))`

#####filter
`(All [a] (-> (-> a lux;Bool) (Stream a) (Stream a)))`

#####partition
`(All [a] (-> (-> a lux;Bool) (Stream a) (, (Stream a) (Stream a))))`

#####Stream/Functor
`(lux/control/functor;Functor Stream)`

#####Stream/CoMonad
`(lux/control/comonad;CoMonad Stream)`

___

#lux/codata/reader
###Types
#####Reader
```
(All [a b] (-> a b))
```





###Values
#####Reader/Functor
`(All [a] (lux/control/functor;Functor (Reader a)))`

#####Reader/Monad
`(All [a] (lux/control/monad;Monad (Reader a)))`

___

#lux/codata/state
###Types
#####State
```
(All [a b] (-> a (, a b)))
```





###Values
#####State/Functor
`(All [a] (lux/control/functor;Functor (State a)))`

#####State/Monad
`(All [a] (lux/control/monad;Monad (State a)))`

#####run-state
`(All [a b] (-> a (State a b) b))`

___

#lux/codata/io
###Types
#####IO
```
(All [a] (-> Void a))
```

###Macros
#####io
Example(s):
```
(io (exec
      (print! msg)
      "Some value..."))
```



###Values
#####IO/Functor
`(lux/control/functor;Functor IO)`

#####IO/Monad
`(lux/control/monad;Monad IO)`

#####run-io
`(All [a] (-> (IO a) a))`

___

#lux/meta/type






###Values
#####Type/Show
`(lux/control/show;Show lux;Type)`

#####Type/Eq
`(lux/control/eq;Eq lux;Type)`

#####beta-reduce
`(-> (lux;List lux;Type) lux;Type lux;Type)`

#####apply-type
`(-> lux;Type lux;Type (lux;Maybe lux;Type))`

___

#lux/host/jvm
###Types
#####Array
```
(All [a] (^ #Array a))
```

###Macros
#####object
Example(s):
```
(object java.lang.Object [(io.vertx.core.Handler io.vertx.core.buffer.Buffer)]
    []
    (#override (io.vertx.core.Handler A) handle [] [(body A)] void
               (run-io (k body)))
    )
```

#####program
Example(s):
```
(program args
  (do IO/Monad
    [app-state &&state;gen-state]
    (&&server-host;deploy-server &&util;server-port (handler app-state))))
```

#####null
Example(s):
```
(null)
```

#####null?
Example(s):
```
(null? object)
```

#####jvm-import
Example(s):
```
(jvm-import io.vertx.core.Vertx
  (#static vertx [] [] io.vertx.core.Vertx #io)
  (createHttpServer [] [] io.vertx.core.http.HttpServer #io)
  (deployVerticle   [] [io.vertx.core.Verticle] void #io))
```

#####array-length
Example(s):
```
(array-length my-array)
```



###Values
#####throwable->text
`(-> (^ java.lang.Throwable) lux;Text)`

___

#lux/host/io


###Values
#####print-char
`(-> lux;Char (lux/codata/io;IO Unit))`

#####print
`(-> lux;Text (lux/codata/io;IO Unit))`

#####print-line
`(-> lux;Text (lux/codata/io;IO Unit))`

#####read-char
`(lux/codata/io;IO (lux;Maybe lux;Char))`

#####read-line
`(lux/codata/io;IO (lux;Maybe lux;Text))`

___

#lux/concurrency/async
###Types
#####Async
```
(All [a] (^ lux.concurrency.async.JvmAsync a))
```

###Macros
#####async
Example(s):
```
## Creates an instance of Async
(async)
```



###Values
#####Async/Functor
`(lux/control/functor;Functor Async)`

#####Async/Monad
`(lux/control/monad;Monad Async)`

#####future
`(All [a] (-> (lux/codata/io;IO a) (Async a)))`

#####wait
`(-> lux;Int (Async Unit))`

#####&!
`(All [a b] (-> (Async a) (Async b) (Async (, a b))))`

#####|!
`(All [a b] (-> (Async a) (Async b) (Async (lux;Either a b))))`

#####time-out!
`(All [a] (-> lux;Int (Async a) (Async (lux;Maybe a))))`

#####query
`(All [a] (-> (Async a) (lux;Maybe a)))`

#####resolve
`(All [a] (-> a (Async a) (lux/codata/io;IO lux;Bool)))`

#####delay
`(All [a] (-> lux;Int a (Async a)))`

___

#lux/concurrency/frp
###Types
#####Chan
```
(All [a] (lux/concurrency/async;Async (lux;Maybe (, a (Chan a)))))
```

###Macros
#####chan
Example(s):
```
## Creates an instance of Chan
(chan)
```



###Values
#####poll
`(All [a] (-> lux;Int (lux/codata/io;IO a) (Chan a)))`

#####filter
`(All [a] (-> (-> a lux;Bool) (Chan a) (Chan a)))`

#####write
`(All [a] (-> a (Chan a) (lux/codata/io;IO (lux;Maybe (Chan a)))))`

#####pipe
`(All [a] (-> (Chan a) (Chan a) (lux/concurrency/async;Async Unit)))`

#####merge
`(All [a] (-> (lux;List (Chan a)) (Chan a)))`

#####foldL
`(All [a b] (-> (-> a b a) a (Chan b) (lux/concurrency/async;Async a)))`

#####no-dups
`(All [a] (-> (lux/control/eq;Eq a) (Chan a) (Chan a)))`

#####Chan/Functor
`(lux/control/functor;Functor Chan)`

___

#lux/math






###Values
#####e
`lux;Real`

#####pi
`lux;Real`

#####cos
`(-> lux;Real lux;Real)`

#####sin
`(-> lux;Real lux;Real)`

#####tan
`(-> lux;Real lux;Real)`

#####acos
`(-> lux;Real lux;Real)`

#####asin
`(-> lux;Real lux;Real)`

#####atan
`(-> lux;Real lux;Real)`

#####cosh
`(-> lux;Real lux;Real)`

#####sinh
`(-> lux;Real lux;Real)`

#####tanh
`(-> lux;Real lux;Real)`

#####ceil
`(-> lux;Real lux;Real)`

#####floor
`(-> lux;Real lux;Real)`

#####exp
`(-> lux;Real lux;Real)`

#####log
`(-> lux;Real lux;Real)`

#####cbrt
`(-> lux;Real lux;Real)`

#####sqrt
`(-> lux;Real lux;Real)`

#####->degrees
`(-> lux;Real lux;Real)`

#####->radians
`(-> lux;Real lux;Real)`

#####round
`(-> lux;Real lux;Int)`

#####atan2
`(-> lux;Real lux;Real lux;Real)`

#####pow
`(-> lux;Real lux;Real lux;Real)`

#####gcd
`(-> lux;Int lux;Int lux;Int)`

#####lcm
`(-> lux;Int lux;Int lux;Int)`