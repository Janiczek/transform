module Example exposing
    ( Expr(..)
    , recurse
    , simplifiedExpr
    , simplifyAll
    , simplifyDoubleNegate
    , simplifyNegate
    , simplifyPlus
    , testExpr
    )

import Transform


{-| Our recursive data structure. Imagine an AST inside a compiler.
-}
type Expr
    = Int_ Int
    | Negate Expr
    | Plus Expr Expr
    | List_ (List Expr)


{-| Shows the `Transform.transform*` functions how to recurse to children.
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



{- TRANSFORMATIONS

   We have opted for `Expr -> Expr` transformations here, but they are
   interchangeable with `Expr -> Maybe Expr` ones using `Transform.toMaybe`
   and `Transform.fromMaybe`.

-}


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



-- COMBINING THE TRANSFORMATIONS


{-| Here we combine all the transformations into one.

As said in the docs, don't use this combined transformation with
`Transform.transformOnce`, instead use `Transform.transformAll`.

-}
simplifyAll : Expr -> Maybe Expr
simplifyAll =
    Transform.orList_
        [ simplifyDoubleNegate
        , simplifyNegate
        , simplifyPlus
        ]



-- RUNNING THE TRANSFORMATION


testExpr : Expr
testExpr =
    List_
        [ -- simple (top-level) cases
          Int_ 1
        , Negate
            (Int_ 2)
        , Negate
            (Negate
                (Int_ 3)
            )
        , Plus
            (Int_ 1)
            (Int_ 3)
        , -- recursive cases
          Negate
            (Negate
                (Negate
                    (Int_ 5)
                )
            )
        , Plus
            (Negate
                (Int_ 1)
            )
            (Negate
                (Int_ 5)
            )
        , Negate
            (Plus
                (Int_ 1)
                (Int_ 6)
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


{-| Returns:

    List_
        [ Int_ 1
        , Int_ -2
        , Int_ 3
        , Int_ 4
        , Int_ -5
        , Int_ -6
        , Int_ -7
        , List_
            [ Int_ -8
            , Int_ 9
            ]
        ]

-}
simplifiedExpr : Expr
simplifiedExpr =
    Transform.transformAll
        recurse
        simplifyAll
        testExpr
