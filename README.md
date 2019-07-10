# Transform

**tl;dr:** Transform your data structures recursively from the bottom up. Very
useful eg. for writing compiler passes.

Inspired by [various](https://twitter.com/puffnfresh/status/1080328018181025792) [tweets](https://twitter.com/acid2/status/1095481220153204736) and my use for something like this in [`elm-in-elm`](https://github.com/elm-in-elm/compiler).

----

Let's say you're writing a compiler and have this AST data structure:

```elm
type Expr
    = Int_ Int
    | Negate Expr
    | Plus Expr Expr
    | List_ (List Expr)
```

You also have a few optimizations your compiler can do:

```elm
simplifyDoubleNegate : Expr -> Expr
simplifyDoubleNegate expr =
    case expr of
        Negate (Negate (Int_ int)) ->
            Int_ int

        _ ->
            expr


simplifyNegate : Expr -> Expr
simplifyNegate expr =
    case expr of
        Negate (Int_ int) ->
            Int_ (negate int)

        _ ->
            expr


simplifyPlus : Expr -> Expr
simplifyPlus expr =
    case expr of
        Plus (Int_ a) (Int_ b) ->
            Int_ (a + b)

        _ ->
            expr
```

And let's say we've got this AST:

```elm
ast : Expr
ast =
  List_
    [ Negate
            (Plus
                (Int_ 1)
                (Negate (Int_ 8))
            )
        , List_
            [ Negate
                (Int_ 8)
            , Plus
                (Int_ 1)
                (Negate
                    (Negate
                        (Int_ 8)
                    )
                )
            ]
        ]
```

Now, the burning question is, how do we run these recursively on the whole AST? How do we make sure there are no gains to be had if we run one more optimization somewhere?

This package has just what you need. Watch!

----

- First, we'll say how and which children we want to recurse to.
- Then, we'll combine our transformations in an (almost) order-independent way. (Well, it will be much better our way than by just composing them.)
- Then, as the last step, we'll run the transformations as long as they actually change the data structure.

```elm
{-| FIRST: say how to recurse!
-}
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
```

```elm
{-| SECOND: combine the transformations!
-}
simplifyAll : Expr -> Maybe Expr
simplifyAll =
    Transform.orList_
        [ simplifyDoubleNegate
        , simplifyNegate
        , simplifyPlus
        ]
```

```elm
{-| THIRD: run the transformations until nothing changes anymore!
-}
simplifiedAst : Expr
simplifiedAst =
    Transform.transformAll
        recurse
        simplifyAll
        ast
```

The result?

```
simplifiedAst
--> List_ [Int_ 7, List_ [Int_ -8, Int_ 9]]
```

(By the way, the functions here are general enough to work on anything, not just
on custom types. Feel free to use it on records, for example!)

----

Another thing this library can do is use a similar recursion helper to get
all the descendants (children, their children, etc.).

```elm
recursiveChildren : (Expr -> List Expr) -> Expr -> List Expr
recursiveChildren fn expr =
    case expr of
        Int_ int ->
            []

        Negate e ->
            fn e

        Plus left right ->
            fn left ++ fn right

        List_ es ->
            List.concatMap fn es


children
    recursiveChildren
    (Plus (Int_ 1) (Negate (Int_ 10)))
-->
    [ Plus (Int_ 1) (Int_ -10)
    , Int_ 1
    , Negate (Int_ 2)
    , Int_ 2
    ]
```

As you can see, the result of calling `children` is all the descendants of the
original expression, in the depth order.

----

Conceptually, this is a spiritual equivalent of [`Control.Lens.Plated`](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html) from Haskell.

Because we don't have auto-derived lenses, it needs you to give it that `recurse` function.

In the future, there are other possible functions to add, like getting the immediate children or all the descendants. Let me know if those would be useful for you!
