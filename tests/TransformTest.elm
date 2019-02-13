module TransformTest exposing
    ( fromMaybeTest
    , orListTest
    , orList_Test
    , toMaybeTest
    , transformAllTest
    , transformOnceTest
    )

import Example
    exposing
        ( Expr(..)
        , simplifyDoubleNegate
        , simplifyNegate
        , simplifyPlus
        )
import Expect
import Fuzz exposing (Fuzzer)
import Random exposing (Generator)
import Random.List
import Shrink exposing (Shrinker)
import Test exposing (..)
import Transform


transformOnceTest : Test
transformOnceTest =
    describe "Transform.transformOnce"
        [ test "example from docs" <|
            \() ->
                Transform.transformOnce
                    Example.recurse
                    simplifyNegate
                    (Plus (Int_ 1) (Negate (Int_ 10)))
                    |> Expect.equal (Plus (Int_ 1) (Int_ -10))
        , fuzz2
            exprFuzzer
            transformationsListFuzzer
            "(this should fail?) transformOnce is okay with composed functions if only shrinking"
          <|
            \expr transformationsList ->
                Transform.transformOnce
                    Example.recurse
                    (List.foldl (>>) identity transformationsList)
                    expr
                    |> Expect.equal
                        (Transform.transformAll
                            Example.recurse
                            (Transform.orList_ transformationsList)
                            expr
                        )

        -- TODO test that it's not OK with composed functions if we're not only shrinking but also expanding
        ]


transformAllTest : Test
transformAllTest =
    describe "Transform.transformAll"
        [ test "example from README" <|
            \() ->
                Transform.transformAll
                    Example.recurse
                    Example.simplifyAll
                    (List_
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
                    )
                    |> Expect.equal
                        (List_
                            [ Int_ 7
                            , List_
                                [ Int_ -8, Int_ 9 ]
                            ]
                        )
        , fuzz2 exprFuzzer transformationsListFuzzer "result of transformAll doesn't change if run through one more transformOnce" <|
            \expr transformationsList ->
                let
                    afterAll =
                        Transform.transformAll
                            Example.recurse
                            (Transform.orList_ transformationsList)
                            expr
                in
                afterAll
                    |> Transform.transformOnce
                        Example.recurse
                        (List.foldl (>>) identity transformationsList)
                    |> Expect.equal afterAll
        , fuzz2 exprFuzzer transformationsListFuzzer "result of transformAll doesn't change if run through one more transformAll" <|
            \expr transformationsList ->
                let
                    afterAll =
                        Transform.transformAll
                            Example.recurse
                            (Transform.orList_ transformationsList)
                            expr
                in
                afterAll
                    |> Transform.transformAll
                        Example.recurse
                        (Transform.orList_ transformationsList)
                    |> Expect.equal afterAll
        ]


orListTest : Test
orListTest =
    describe "Transform.orList"
        [ fuzz exprFuzzer "orList [a,b] ~= or a b" <|
            \expr ->
                let
                    orList =
                        Transform.orList (List.map Transform.toMaybe [ simplifyNegate, simplifyPlus ])

                    or =
                        Transform.or
                            (Transform.toMaybe simplifyNegate)
                            (Transform.toMaybe simplifyPlus)
                in
                Transform.transformAll Example.recurse orList expr
                    |> Expect.equal (Transform.transformAll Example.recurse or expr)
        , fuzz exprFuzzer "orList [a,b,c] ~= or (or a b) c" <|
            \expr ->
                let
                    orList =
                        Transform.orList
                            (List.map Transform.toMaybe
                                [ simplifyDoubleNegate
                                , simplifyNegate
                                , simplifyPlus
                                ]
                            )

                    or =
                        Transform.or
                            (Transform.or
                                (Transform.toMaybe simplifyDoubleNegate)
                                (Transform.toMaybe simplifyNegate)
                            )
                            (Transform.toMaybe simplifyPlus)
                in
                Transform.transformAll Example.recurse orList expr
                    |> Expect.equal (Transform.transformAll Example.recurse or expr)
        ]


orList_Test : Test
orList_Test =
    describe "Transform.orList_"
        [ fuzz2 exprFuzzer transformationsListFuzzer "orList_ list ~= orList (List.map toMaybe list)" <|
            \expr transformationsList ->
                let
                    orList_ =
                        Transform.orList_ transformationsList

                    orList =
                        Transform.orList (List.map Transform.toMaybe transformationsList)
                in
                Transform.transformAll Example.recurse orList_ expr
                    |> Expect.equal (Transform.transformAll Example.recurse orList expr)
        , fuzz3
            exprFuzzer
            combinedTransformationFuzzer
            combinedTransformationFuzzer
            "(this should fail?) calling orList_ with differently shuffled transformations shouldn't change the end result"
          <|
            \expr fn1 fn2 ->
                Transform.transformAll Example.recurse fn1 expr
                    |> Expect.equal (Transform.transformAll Example.recurse fn2 expr)
        ]


toMaybeTest : Test
toMaybeTest =
    describe "Transform.toMaybe"
        [ fuzz2 exprFuzzer transformationFuzzer "if result /= input, Just result, else Nothing" <|
            \expr transformation ->
                let
                    resultWithout =
                        transformation expr

                    resultWith =
                        Transform.toMaybe transformation expr
                in
                case ( expr == resultWithout, resultWith ) of
                    ( False, Just resultWith_ ) ->
                        resultWith_
                            |> Expect.equal resultWithout

                    ( True, Nothing ) ->
                        Expect.pass

                    _ ->
                        Expect.fail "Shouldn't happej"
        , fuzz2 exprFuzzer transformationFuzzer "toMaybe >> fromMaybe ~= identity" <|
            \expr transformation ->
                (transformation
                    |> Transform.toMaybe
                    |> Transform.fromMaybe
                )
                    expr
                    |> Expect.equal (transformation expr)
        ]


fromMaybeTest : Test
fromMaybeTest =
    describe "Transform.fromMaybe"
        [ fuzz2 exprFuzzer maybeTransformationFuzzer "if Nothing, input, else Just result" <|
            \expr maybeTransformation ->
                let
                    resultWithout =
                        maybeTransformation expr

                    resultWith =
                        Transform.fromMaybe maybeTransformation expr
                in
                case ( resultWithout, expr == resultWith ) of
                    ( Nothing, True ) ->
                        Expect.pass

                    ( Just result, False ) ->
                        result
                            |> Expect.equal resultWith

                    _ ->
                        Expect.fail "Shouldn't happen"
        , fuzz2 exprFuzzer maybeTransformationFuzzer "fromMaybe >> toMaybe ~= identity" <|
            \expr maybeTransformation ->
                (maybeTransformation
                    |> Transform.fromMaybe
                    |> Transform.toMaybe
                )
                    expr
                    |> Expect.equal (maybeTransformation expr)
        ]


exprFuzzer : Fuzzer Expr
exprFuzzer =
    Fuzz.custom
        exprGenerator
        exprShrinker


transformationFuzzer : Fuzzer (Expr -> Expr)
transformationFuzzer =
    allTransformations
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


maybeTransformationFuzzer : Fuzzer (Expr -> Maybe Expr)
maybeTransformationFuzzer =
    transformationFuzzer
        |> Fuzz.map Transform.toMaybe


transformationsListGenerator : Generator (List (Expr -> Expr))
transformationsListGenerator =
    Random.List.shuffle allTransformations


transformationsListFuzzer : Fuzzer (List (Expr -> Expr))
transformationsListFuzzer =
    Fuzz.custom
        transformationsListGenerator
        Shrink.noShrink


combinedTransformationFuzzer : Fuzzer (Expr -> Maybe Expr)
combinedTransformationFuzzer =
    Fuzz.custom
        (Random.map Transform.orList_ transformationsListGenerator)
        Shrink.noShrink


allTransformations : List (Expr -> Expr)
allTransformations =
    [ simplifyDoubleNegate
    , simplifyNegate
    , simplifyPlus
    ]


exprGenerator : Generator Expr
exprGenerator =
    let
        lazyExprGenerator =
            Random.lazy (\() -> exprGenerator)
    in
    -- TODO We're overflowing the stack... :( elm-test, get your stuff together!
    Random.uniform
        (Random.map Int_ (Random.int Random.minInt Random.maxInt))
        [ Random.map Negate lazyExprGenerator
        , Random.map2 Plus lazyExprGenerator lazyExprGenerator
        , Random.int 0 2
            |> Random.andThen (\length -> Random.map List_ (Random.list length lazyExprGenerator))
        ]
        |> Random.andThen identity


exprShrinker : Shrinker Expr
exprShrinker expr =
    let
        nestedExpr : Shrinker Expr
        nestedExpr =
            Shrink.merge
                exprShrinker
                Shrink.noShrink
    in
    case expr of
        Int_ int ->
            Shrink.map Int_ (Shrink.int int)

        Negate e ->
            nestedExpr e

        Plus e1 e2 ->
            Shrink.merge
                (always (nestedExpr e1))
                (always (nestedExpr e2))
                expr

        List_ es ->
            Shrink.list nestedExpr es
                |> Shrink.map List_


maybeExtraOr : Maybe a -> Maybe a -> Maybe a
maybeExtraOr a b =
    if a == Nothing then
        b

    else
        a
