module Transform exposing
    ( transformOnce, transformAll
    , or, orList, orList_
    , toMaybe, fromMaybe
    )

{-| Transform your data structures recursively from the bottom up. Very useful
eg. for writing compiler passes.

---

Conceptually, it's a spiritual equivalent of [`Control.Lens.Plated`](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html) from Haskell.

Because we don't have auto-derived lenses, it needs you to give it a function
that encapsulates where your data structure recurses in addition to the actual
transformation function you want to run at all levels of the data structure.


# Running transformations recursively

@docs transformOnce, transformAll


# Combining transformations

@docs or, orList, orList_


# Converting transformations

@docs toMaybe, fromMaybe

-}


{-| Runs the transformation function on **all** the nodes of your recursive
data structure **from the bottom up.** Runs only once, hence the name!

    transformOnce
        recurse
        simplifyNegate
        (Plus (Int_ 1) (Negate (Int_ 10)))
    --> Plus (Int_ 1) (Int_ -10)

The `recurse` function tells the `transform*` functions which children
to recurse to. In our example it looks like this:

    recurse : (Expr -> Expr) -> Expr -> Expr
    recurse fn expr =
        case expr of
            Int_ int ->
                Int_ int

            Negate e ->
                Negate (fn e)

            Plus left right ->
                Plus (fn left) (fn right)

            List_ es ->
                List_ (List.map fn es)

But you could of course make it skip some of the children by not applying the function on them.

---

**WARNING:** I'm not 100% sure but I think using this function with composed
transformations (`simplifyNegate >> simplifyPlus`) _that don't just shrink but
also expand_ might not always lead to the fixpoint. To be sure, **use
`transformAll`** and transformations combined with `orList` or a similar helper
function.

---

An equivalent of [`Control.Lens.Plated.transform`](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html#v:transform). (Hopefully.)

-}
transformOnce : ((a -> a) -> a -> a) -> (a -> a) -> a -> a
transformOnce recurse fn expr =
    let
        helper expr_ =
            (fn << recurse helper) expr_
    in
    helper expr


{-| Runs the transformation function on **all** the nodes of your recursive
data structure **from the bottom up,** stopping only when there's no more
transformations applicable anywhere in the tree.

**The transformation function has to return `Nothing` if there's nothing
to be done.** It's possible to create an infinite loop with a transformation
that always returns `Just`!

    transformAll
        recurse
        maybeSimplifyNegate
        (Plus (Int_ 1) (Negate (Int_ 10)))
    --> Plus (Int_ 3) (Int_ -10)

    transformAll
        recurse
        (orList_ [simplifyNegate, simplifyPlus])
        (Plus (Int_ 1) (Negate (Int_ 10)))
    --> Int_ -7

---

An equivalent of [`Control.Lens.Plated.rewrite`](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html#v:rewrite). (Hopefully.)

-}
transformAll : ((a -> a) -> a -> a) -> (a -> Maybe a) -> a -> a
transformAll recurse fn expr =
    let
        helper expr_ =
            transformOnce recurse
                (\x ->
                    fn x
                        |> Maybe.map helper
                        |> Maybe.withDefault x
                )
                expr_
    in
    helper expr


{-| [`Maybe.Extra.or`](https://package.elm-lang.org/packages/elm-community/maybe-extra/latest/Maybe-Extra#or) made to work with the type signatures `transformAll` expects.

**The argument order** (and thus, whether you use this function in a pipeline or not)
**doesn't matter** because `transformAll` literally tries to run the given
transformation as long as it changes stuff (ie. until the fixed point is reached).

The way the transformations are combined makes sure that Nothing is returned only
if **no** transformation can be applied anymore.

    simplifyNegateOrPlus : Expr -> Maybe Expr
    simplifyNegateOrPlus =
        or
            maybeSimplifyNegate
            maybeSimplifyPlus

---

Conceptually, this is `mplus` of the `Alternative` instance for `(a -> Maybe a)`.

-}
or : (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
or leftFn rightFn value =
    let
        left =
            leftFn value
    in
    if left == Nothing then
        rightFn value

    else
        left


{-| A nicer way to combine transformations for `transformAll`.

As with `transformAll`, the argument order (and thus, whether you use this
function in a pipeline or not) doesn't matter because `transformAll` literally
tries to run the given transformation as long as it changes stuff (ie. until
the fixed point is reached).

The way the transformations are combined makes sure that Nothing is returned only
if **no** transformation can be applied anymore.

    simplifyAll : Expr -> Maybe Expr
    simplifyAll =
        orList
            [ maybeSimplifyDoubleNegate
            , maybeSimplifyNegate
            , maybeSimplifyPlus
            ]

---

Again, conceptually, this is the `mplus` of the `Alternative` instance
for `(a -> Maybe a)`, combined with a fold.

-}
orList : List (a -> Maybe a) -> a -> Maybe a
orList fns =
    List.foldl or (always Nothing) fns


{-| A variant of `orList` for non-Maybe transformations.

    orList_ : List (a -> a) -> a -> Maybe a
    orList_ fns =
        orList (List.map toMaybe fns)

    simplifyAll : Expr -> Maybe Expr
    simplifyAll =
        orList_
            [ simplifyDoubleNegate
            , simplifyNegate
            , simplifyPlus
            ]

-}
orList_ : List (a -> a) -> a -> Maybe a
orList_ fns =
    orList (List.map toMaybe fns)


{-| Modifies the transformation to return Nothing if it doesn't change the value.

    simplifyNegate : Expr -> Expr
    simplifyNegate expr =
        case expr of
            Negate (Int_ int) ->
                Int_ (negate int)

            _ ->
                expr

    maybeSimplifyNegate : Expr -> Maybe Expr
    maybeSimplifyNegate =
        toMaybe simplifyNegate

    simplifyNegate      (Int_ 42) --> Int_ 42
    maybeSimplifyNegate (Int_ 42) --> Nothing

    simplifyNegate      (Negate (Int_ 42)) --> Int_ -42
    maybeSimplifyNegate (Negate (Int_ 42)) --> Just (Int_ -42)

-}
toMaybe : (a -> a) -> a -> Maybe a
toMaybe fn value =
    let
        result =
            fn value
    in
    if result == value then
        Nothing

    else
        Just result


{-| Modifies the transformation to return the original input if it returns Nothing.

    maybeSimplifyNegate : Expr -> Maybe Expr
    maybeSimplifyNegate expr =
        case expr of
            Negate (Int_ int) ->
                Just (Int_ (negate int))

            _ ->
                Nothing

    simplifyNegate : Expr -> Expr
    simplifyNegate =
        fromMaybe maybeSimplifyNegate

    maybeSimplifyNegate (Int_ 42) --> Nothing
    simplifyNegate      (Int_ 42) --> Int_ 42

    maybeSimplifyNegate (Negate (Int_ 42)) --> Just (Int_ -42)
    simplifyNegate      (Negate (Int_ 42)) --> Int_ -42

-}
fromMaybe : (a -> Maybe a) -> a -> a
fromMaybe fn value =
    fn value
        |> Maybe.withDefault value
