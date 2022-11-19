module TransformTest exposing
    ( childrenTest
    , fromMaybeTest
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
                            , Negate (Negate (Int_ 3))
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
                                [ Int_ -8, Int_ 3, Int_ 9 ]
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
        , test "Example from Example.elm" <|
            \() ->
                Example.simplifiedExpr
                    |> Expect.equal
                        (List_ [ Int_ 1, Int_ -2, Int_ 3, Int_ 4, Int_ -5, Int_ -6, Int_ -7, List_ [ Int_ -8, Int_ 9 ] ])
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


transformationFuzzer : Fuzzer (Expr -> Expr)
transformationFuzzer =
    allTransformations
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


maybeTransformationFuzzer : Fuzzer (Expr -> Maybe Expr)
maybeTransformationFuzzer =
    transformationFuzzer
        |> Fuzz.map Transform.toMaybe


transformationsListFuzzer : Fuzzer (List (Expr -> Expr))
transformationsListFuzzer =
    Fuzz.shuffledList allTransformations


combinedTransformationFuzzer : Fuzzer (Expr -> Maybe Expr)
combinedTransformationFuzzer =
    Fuzz.map Transform.orList_ transformationsListFuzzer


allTransformations : List (Expr -> Expr)
allTransformations =
    [ simplifyDoubleNegate
    , simplifyNegate
    , simplifyPlus
    ]


exprDepthLimit =
    10


exprFuzzer : Fuzzer Expr
exprFuzzer =
    exprFuzzer_ 0


exprFuzzer_ : Int -> Fuzzer Expr
exprFuzzer_ n =
    let
        e =
            Fuzz.lazy (\() -> exprFuzzer_ (n + 1))
    in
    if n == exprDepthLimit then
        -- no more branching
        intFuzzer

    else
        Fuzz.oneOf
            [ intFuzzer
            , negateFuzzer e
            , plusFuzzer e
            , listFuzzer e
            ]


intFuzzer =
    Fuzz.map Int_ Fuzz.int


negateFuzzer e =
    Fuzz.map Negate e


plusFuzzer e =
    Fuzz.map2 Plus e e


listFuzzer e =
    Fuzz.listOfLengthBetween 0 3 e
        |> Fuzz.map List_


maybeExtraOr : Maybe a -> Maybe a -> Maybe a
maybeExtraOr a b =
    if a == Nothing then
        b

    else
        a


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


smallExpr : Expr
smallExpr =
    Plus
        (Int_ 1)
        (Negate (Int_ 2))


childrenTest : Test
childrenTest =
    describe "Transform.children"
        [ test "example from README" <|
            \() ->
                Transform.children
                    recursiveChildren
                    smallExpr
                    |> Expect.equal
                        [ smallExpr
                        , Int_ 1
                        , Negate (Int_ 2)
                        , Int_ 2
                        ]
        ]
