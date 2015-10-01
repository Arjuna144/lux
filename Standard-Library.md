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

#####Unit
```
(,)
```

#####Void
```
(|)
```

#####Ident
```
(, Text Text)
```

#####List
```
(All [a]
  (| #Nil
     (#Cons a (List a))))
```

#####Maybe
```
(All [a]
  (| #None
     (#Some a)))
```

#####Type
```
((All [a]
  (| (#DataT Text (List (Type a)))
     (#VariantT (List (Type a)))
     (#TupleT (List (Type a)))
     (#LambdaT (Type a) (Type a))
     (#BoundT Int)
     (#VarT Int)
     (#ExT Int)
     (#UnivQ (List (Type a)) (Type a))
     (#ExQ (List (Type a)) (Type a))
     (#AppT (Type a) (Type a))
     (#NamedT Ident (Type a)))) Void)
```

#####Bindings
```
(All [a b]
  (& #counter Int
     #mappings (List (, a b))))
```

#####Env
```
(All [a b]
  (& #name Text
     #inner-closures Int
     #locals (Bindings a b)
     #closure (Bindings a b)))
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
  (& #meta a
     #datum b))
```

#####AST'
```
(All [a]
  (| (#BoolS Bool)
     (#IntS Int)
     (#RealS Real)
     (#CharS Char)
     (#TextS Text)
     (#SymbolS Ident)
     (#TagS Ident)
     (#FormS (List (a (AST' a))))
     (#TupleS (List (a (AST' a))))
     (#RecordS (List (, (a (AST' a)) (a (AST' a)))))))
```

#####AST
```
(Meta Cursor (AST' (Meta Cursor)))
```

#####Either
```
(All [a b]
  (| (#Left a)
     (#Right b)))
```

#####Source
```
(List (Meta Cursor Text))
```

#####DefData'
```
(All [a] (| (, Type Unit) Type a Ident))
```

#####Analysis
```
Void
```

#####Module
```
(All [a]
  (& #module-aliases (List (, Text Text))
     #defs (List (, Text (, Bool (DefData' (-> (List AST) ((All [b c] (-> b (Either Text (, b c)))) a (List AST)))))))
     #imports (List Text)
     #tags (List (, Text (, Int (List Ident) Type)))
     #types (List (, Text (, (List Ident) Type)))))
```

#####Compiler
```
((All [a]
  (& #source Source
     #cursor Cursor
     #modules (List (, Text (Module (Compiler a))))
     #envs (List (Env Text (Meta (, Type Cursor) Analysis)))
     #type-vars (Bindings Int Type)
     #expected Type
     #seed Int
     #eval? Bool
     #host Void)) Void)
```

#####Macro
```
(-> (List AST) ((All [a b] (-> a (Either Text (, a b)))) Compiler (List AST)))
```

#####DefData
```
(DefData' Macro)
```

#####Definition
```
(Meta Bool DefData)
```

#####Lux
```
(All [a] (-> Compiler (Either Text (, Compiler a))))
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

(,) ## The empty tuple, aka "unit"
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

#####deftype
Example(s):
```
## The type-definition macro
(deftype (List a)
  (| #Nil
     (#Cons (, a (List a)))))
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

#####using
Example(s):
```
## Opens up a structure and provides all the definitions as local variables.
(using Int/Ord
  (< 5 10))
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
    [[#;VariantT]
     [#;TupleT]]

    (\template [<tag>]
     [(<tag> left right)
      (<tag> (beta-reduce env left) (beta-reduce env right))])
    [[#;LambdaT]
     [#;AppT]]

    (\template [<tag>]
     [(<tag> env def)
      (case env
        #;Nil
        (<tag> env def)

        _
        type)])
    [[#;UnivQ]
     [#;ExQ]]
    
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

#####export
Example(s):
```
(export Lux/Monad modules find-in-env)
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
  (& #unit a
     #++ (-> a a a)))
```





###Values
#####monoid$
`(All [a] (-> a (-> a a a) (Monoid a)))`

___

#lux/control/functor
###Types
#####Functor
```
(All [f]
  (& #map (All [b c] (-> (-> b c) (f b) (f c)))))
```







___

#lux/control/monad
###Types
#####Monad
```
(All [m]
  (& #_functor (lux/control/functor;Functor m)
     #wrap (All [b] (-> b (m b)))
     #join (All [b] (-> (m (m b)) (m b)))))
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
`(All [m b c] (-> (Monad m) (-> b (m c)) (m b) (m c)))`

#####seq%
`(All [m b] (-> (Monad m) (lux;List (m b)) (m (lux;List b))))`

#####map%
`(All [m b c] (-> (Monad m) (-> b (m c)) (lux;List b) (m (lux;List c))))`

___

#lux/control/comonad
###Types
#####CoMonad
```
(All [w]
  (& #_functor (lux/control/functor;Functor w)
     #unwrap (All [b] (-> (w b) b))
     #split (All [b] (-> (w b) (w (w b))))))
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
`(All [w b c] (-> (CoMonad w) (-> (w b) c) (w b) (w c)))`

___

#lux/data/list


###Macros
#####@list
Example(s):
```
(@list 1 2 3)
```

#####@list&
Example(s):
```
(@list& 1 2 3 (@list 4 5 6))
```

#####zip
Example(s):
```
((zip 3) xs ys zs)
```



###Values
#####List/Fold
`(lux/control/fold;Fold lux;List)`

#####fold
`(All [a] (-> (lux/control/monoid;Monoid a) (lux;List a) a))`

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

#####zip2
`(All [a b] (-> (lux;List a) (lux;List b) (lux;List (, a b))))`

#####zip3
`(All [a b c] (-> (lux;List a) (lux;List b) (lux;List c) (lux;List (, a b c))))`

#####empty?
`(All [a] (-> (lux;List a) lux;Bool))`

___

#lux/control/eq
###Types
#####Eq
```
(All [a]
  (& #= (-> a a lux;Bool)))
```







___

#lux/control/ord
###Types
#####Ord
```
(All [a]
  (& #_eq (lux/control/eq;Eq a)
     #< (-> a a lux;Bool)
     #<= (-> a a lux;Bool)
     #> (-> a a lux;Bool)
     #>= (-> a a lux;Bool)))
```





###Values
#####ord$
`(All [a] (-> (lux/control/eq;Eq a) (-> a a lux;Bool) (-> a a lux;Bool) (Ord a)))`

#####max
`(All [a] (-> (Ord a) a a a))`

#####min
`(All [a] (-> (Ord a) a a a))`

___

#lux/control/fold
###Types
#####Fold
```
(All [f]
  (& #foldL (All [b c] (-> (-> b c b) b (f c) b))
     #foldR (All [b c] (-> (-> c b b) b (f c) b))))
```





###Values
#####foldM
`(All [f b] (-> (lux/control/monoid;Monoid b) (Fold f) (f b) b))`

#####size
`(All [f b] (-> (Fold f) (f b) lux;Int))`

#####member?
`(All [f b] (-> (lux/control/eq;Eq b) (Fold f) b (f b) lux;Bool))`

#####empty?
`(All [f b] (-> (Fold f) (f b) lux;Bool))`

___

#lux/data/number/int






###Values
#####Int/Number
`(lux/control/number;Number lux;Int)`

#####Int/Eq
`(lux/control/eq;Eq lux;Int)`

#####Int/Ord
`(lux/control/ord;Ord lux;Int)`

#####Int/Enum
`(lux/control/enum;Enum lux;Int)`

#####Int/Bounded
`(lux/control/bounded;Bounded lux;Int)`

#####IntAdd/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMul/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMax/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####IntMin/Monoid
`(lux/control/monoid;Monoid lux;Int)`

#####Int/Show
`(lux/control/show;Show lux;Int)`

___

#lux/control/number
###Types
#####Number
```
(All [a]
  (& #+ (-> a a a)
     #- (-> a a a)
     #* (-> a a a)
     #/ (-> a a a)
     #% (-> a a a)
     #negate (-> a a)
     #signum (-> a a)
     #abs (-> a a)
     #from-int (-> lux;Int a)))
```







___

#lux/control/bounded
###Types
#####Bounded
```
(All [a]
  (& #top a
     #bottom a))
```







___

#lux/control/show
###Types
#####Show
```
(All [a]
  (& #show (-> a lux;Text)))
```







___

#lux/control/enum
###Types
#####Enum
```
(All [a]
  (& #_ord (lux/control/ord;Ord a)
     #succ (-> a a)
     #pred (-> a a)))
```





###Values
#####range
`(All [a] (-> (Enum a) a a (lux;List a)))`

___

#lux/data/bool






###Values
#####Bool/Eq
`(lux/control/eq;Eq lux;Bool)`

#####Bool/Show
`(lux/control/show;Show lux;Bool)`

#####Or/Monoid
`(lux/control/monoid;Monoid lux;Bool)`

#####And/Monoid
`(lux/control/monoid;Monoid lux;Bool)`

#####comp
`(All [a] (-> (-> a lux;Bool) a lux;Bool))`

___

#lux/codata/function






###Values
#####const
`(All [a b] (-> a b a))`

#####flip
`(All [a b c] (-> (-> a b c) b a c))`

#####.
`(All [a b c] (-> (-> b c) (-> a b) a c))`

#####Comp/Monoid
`(All [a] (lux/control/monoid;Monoid (-> a a)))`

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

#####sub'
`(-> lux;Int lux;Int lux;Text (lux;Maybe lux;Text))`

#####sub
`(-> lux;Int lux;Text (lux;Maybe lux;Text))`

#####split
`(-> lux;Int lux;Text (lux;Maybe (, lux;Text lux;Text)))`

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

#####Text/Eq
`(lux/control/eq;Eq lux;Text)`

#####Text/Ord
`(lux/control/ord;Ord lux;Text)`

#####Text/Show
`(lux/control/show;Show lux;Text)`

#####Text/Monoid
`(lux/control/monoid;Monoid lux;Text)`

#####split-lines
`(-> lux;Text (lux;List lux;Text))`

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

#lux/control/hash
###Types
#####Hash
```
(All [a]
  (& #hash (-> a lux;Int)))
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

#####Error/Functor
`(All [a] (lux/control/functor;Functor (lux;Either a)))`

#####Error/Monad
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

#lux/data/number/real






###Values
#####Real/Number
`(lux/control/number;Number lux;Real)`

#####Real/Eq
`(lux/control/eq;Eq lux;Real)`

#####Real/Ord
`(lux/control/ord;Ord lux;Real)`

#####Real/Bounded
`(lux/control/bounded;Bounded lux;Real)`

#####RealAdd/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMul/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMax/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####RealMin/Monoid
`(lux/control/monoid;Monoid lux;Real)`

#####Real/Show
`(lux/control/show;Show lux;Real)`

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

#####macro-expand
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####macro-expand-all
`(-> lux;AST (lux;Lux (lux;List lux;AST)))`

#####gensym
`(-> lux;Text (lux;Lux lux;AST))`

#####emit
`(All [a] (-> a (lux;Lux a)))`

#####fail
`(All [a] (-> lux;Text (lux;Lux a)))`

#####macro-expand-1
`(-> lux;AST (lux;Lux lux;AST))`

#####module-exists?
`(-> lux;Text (lux;Lux lux;Bool))`

#####exported-defs
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
`(-> lux;Text (lux;Lux (lux;List (, lux;Text lux;Definition))))`

#####exports
`(-> lux;Text (lux;Lux (lux;List (, lux;Text lux;Definition))))`

#####modules
`(lux;Lux (lux;List lux;Text))`

#####find-module
`(-> lux;Text (lux;Lux (lux;Module lux;Compiler)))`

#####tags-for
`(-> lux;Ident (lux;Lux (lux;Maybe (lux;List lux;Ident))))`

___

#lux/meta/ast






###Values
#####bool$
`(-> lux;Bool lux;AST)`

#####int$
`(-> lux;Int lux;AST)`

#####real$
`(-> lux;Real lux;AST)`

#####char$
`(-> lux;Char lux;AST)`

#####text$
`(-> lux;Text lux;AST)`

#####symbol$
`(-> lux;Ident lux;AST)`

#####tag$
`(-> lux;Ident lux;AST)`

#####form$
`(-> (lux;List lux;AST) lux;AST)`

#####tuple$
`(-> (lux;List lux;AST) lux;AST)`

#####record$
`(-> (lux;List (, lux;AST lux;AST)) lux;AST)`

#####AST/Show
`(lux/control/show;Show lux;AST)`

#####AST/Eq
`(lux/control/eq;Eq lux;AST)`

___

#lux/meta/syntax
###Types
#####Parser
```
(All [a] (-> (lux;List lux;AST) (lux;Maybe (, (lux;List lux;AST) a))))
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

#####int^
`(Parser lux;Int)`

#####real^
`(Parser lux;Real)`

#####char^
`(Parser lux;Char)`

#####text^
`(Parser lux;Text)`

#####symbol^
`(Parser lux;Ident)`

#####tag^
`(Parser lux;Ident)`

#####assert
`(-> lux;Bool (Parser (,)))`

#####nat^
`(Parser lux;Int)`

#####local-symbol^
`(Parser lux;Text)`

#####local-tag^
`(Parser lux;Text)`

#####bool?^
`(-> lux;Bool (Parser lux;Bool))`

#####int?^
`(-> lux;Int (Parser lux;Bool))`

#####real?^
`(-> lux;Real (Parser lux;Bool))`

#####char?^
`(-> lux;Char (Parser lux;Bool))`

#####text?^
`(-> lux;Text (Parser lux;Bool))`

#####symbol?^
`(-> lux;Ident (Parser lux;Bool))`

#####tag?^
`(-> lux;Ident (Parser lux;Bool))`

#####bool!^
`(-> lux;Bool (Parser lux;Unit))`

#####int!^
`(-> lux;Int (Parser lux;Unit))`

#####real!^
`(-> lux;Real (Parser lux;Unit))`

#####char!^
`(-> lux;Char (Parser lux;Unit))`

#####text!^
`(-> lux;Text (Parser lux;Unit))`

#####symbol!^
`(-> lux;Ident (Parser lux;Unit))`

#####tag!^
`(-> lux;Ident (Parser lux;Unit))`

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
`(All [a] (-> (lux;List (Parser a)) (Parser (lux;Maybe a))))`

#####end^
`(Parser (,))`

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
(All [a] (-> (,) a))
```

###Macros
#####@io
Example(s):
```
## Macro for wrapping arbitrary computations inside the IO monad.
(@io (exec
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

#lux/host/jvm


###Macros
#####Array
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####defclass
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####definterface
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####object
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####program
Example(s):
```
(program args
  (write-line "Hello, world!"))
```

#####???
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####try
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####instance?
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####locking
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####null?
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####new$
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####invoke-virtual$
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####invoke-interface$
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####invoke-special$
Example(s):
```
## To be explained in the JVM-interop wiki page
```

#####invoke-static$
Example(s):
```
## To be explained in the JVM-interop wiki page
```



###Values
#####throwable->text
`(-> (^ java.lang.Throwable) lux;Text)`

___

#lux/host/io


###Macros
#####with-open
Example(s):
```
## To be explained in the JVM-interop wiki page
```



###Values
#####write-char
`(-> lux;Char (lux/codata/io;IO (,)))`

#####write
`(-> lux;Text (lux/codata/io;IO (,)))`

#####write-line
`(-> lux;Text (lux/codata/io;IO (,)))`

#####read-char
`(lux/codata/io;IO (lux;Maybe lux;Char))`

#####read-line
`(lux/codata/io;IO (lux;Maybe lux;Text))`

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