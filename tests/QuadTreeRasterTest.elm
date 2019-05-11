module QuadTreeRasterTest exposing (suite)

import Bitwise
import Dict
import Expect
import Fuzz
import QuadTreeRaster as Raster exposing (Size, Token(..))
import QuadTreeRaster.Internal as Internal exposing (Model, Node(..), Raster(..))
import Test exposing (Test)


type Value
    = A
    | B
    | C
    | D


getRoot : Raster a -> Node a
getRoot (Raster model) =
    model.root


suite : Test
suite =
    Test.describe "QuadTreeRaster"
        [ Test.test "init" <|
            \_ ->
                Raster.init (Size 5 10) A
                    |> Expect.equal
                        (Raster
                            { size =
                                { width = 5
                                , height = 10
                                }
                            , quadSizes = [ 8, 4, 2, 1 ]
                            , root = LeafNode A
                            }
                        )
        , Test.describe "size"
            [ Test.test "init getSize" <|
                \_ ->
                    Raster.init (Size 5 10) A
                        |> Raster.getSize
                        |> Expect.equal
                            { width = 5
                            , height = 10
                            }
            , Test.test "init deserialize" <|
                \_ ->
                    Raster.deserialize (Size 5 10) [ Leaf A ]
                        |> Maybe.map Raster.getSize
                        |> Expect.equal
                            (Just
                                { width = 5
                                , height = 10
                                }
                            )
            , Test.fuzz (Fuzz.map2 Size Fuzz.int Fuzz.int) "init size at least 0x0" <|
                \fuzzSize ->
                    Raster.init fuzzSize A
                        |> Raster.getSize
                        |> Expect.all
                            [ .width >> Expect.atLeast 0
                            , .height >> Expect.atLeast 0
                            ]
            , Test.fuzz (Fuzz.map2 Size Fuzz.int Fuzz.int) "deserialize size at least 0x0" <|
                \fuzzSize ->
                    Raster.deserialize fuzzSize [ Leaf A ]
                        |> Maybe.map Raster.getSize
                        |> Maybe.map
                            (Expect.all
                                [ .width >> Expect.atLeast 0
                                , .height >> Expect.atLeast 0
                                ]
                            )
                        |> Maybe.withDefault (Expect.fail "deserialize failed")
            ]
        , Test.describe "quad size"
            [ Test.test "init not power of 2" <|
                \_ ->
                    Internal.initQuadSizes (Size 5 10)
                        |> Expect.equal [ 8, 4, 2, 1 ]
            , Test.test "init power of 2" <|
                \_ ->
                    Internal.initQuadSizes (Size 4 16)
                        |> Expect.equal [ 8, 4, 2, 1 ]
            , Test.test "init 0" <|
                \_ ->
                    Internal.initQuadSizes (Size 0 0)
                        |> Expect.equal []
            , Test.test "init negative" <|
                \_ ->
                    Internal.initQuadSizes (Size -1 -1)
                        |> Expect.equal []
            , Test.test "init  1073741824" <|
                \_ ->
                    Internal.initQuadSizes (Size 1 1073741824)
                        |> List.head
                        |> Expect.equal (Just 536870912)
            , Test.test "init  2147483648" <|
                \_ ->
                    Internal.initQuadSizes (Size 1 2147483648)
                        |> List.head
                        |> Expect.equal (Just 1073741824)
            , Test.test "init  4294967296" <|
                \_ ->
                    Internal.initQuadSizes (Size 1 4294967296)
                        |> List.head
                        |> Expect.equal (Just 2147483648)
            , Test.test "init  8589934592" <|
                \_ ->
                    Internal.initQuadSizes (Size 1 8589934592)
                        |> List.head
                        |> Expect.equal (Just 4294967296)
            , Test.fuzz (Fuzz.map2 Size Fuzz.int Fuzz.int) "init fuzz expect 0 or power of 2" <|
                \fuzzSize ->
                    if fuzzSize.width > 0 && fuzzSize.height > 0 then
                        Internal.initQuadSizes fuzzSize
                            |> List.head
                            |> Maybe.map
                                (Expect.all
                                    [ Expect.lessThan (max fuzzSize.width fuzzSize.height)
                                    , \quadSize ->
                                        Expect.true
                                            (String.fromInt quadSize ++ " is not power of 2")
                                            (isPowerOfTwo quadSize)
                                    ]
                                )
                            |> Maybe.withDefault (Expect.fail "empty quad sizes")

                    else
                        Internal.initQuadSizes fuzzSize
                            |> Expect.equal []
            ]
        , Test.describe "get" <|
            let
                raster =
                    Raster
                        { size =
                            { width = 5
                            , height = 10
                            }
                        , quadSizes = [ 8, 4, 2, 1 ]
                        , root =
                            BranchNode
                                { q1 = LeafNode A
                                , q2 = LeafNode B
                                , q3 = LeafNode C
                                , q4 = LeafNode D
                                }
                        }
            in
            [ Test.test "q1 edge 0 0" (\_ -> Raster.get 0 0 raster |> Expect.equal (Just A))
            , Test.test "q1 edge 1 0" (\_ -> Raster.get 4 0 raster |> Expect.equal (Just A))
            , Test.test "q1 edge 0 1" (\_ -> Raster.get 0 7 raster |> Expect.equal (Just A))
            , Test.test "q1 edge 1 1" (\_ -> Raster.get 4 7 raster |> Expect.equal (Just A))
            , Test.test "q3 edge 0 0" (\_ -> Raster.get 0 8 raster |> Expect.equal (Just C))
            , Test.test "q3 edge 1 0" (\_ -> Raster.get 4 8 raster |> Expect.equal (Just C))
            , Test.test "q3 edge 0 1" (\_ -> Raster.get 0 9 raster |> Expect.equal (Just C))
            , Test.test "q3 edge 1 1" (\_ -> Raster.get 4 9 raster |> Expect.equal (Just C))
            , Test.test "out of bounds left" (\_ -> Raster.get -1 0 raster |> Expect.equal Nothing)
            , Test.test "out of bounds top" (\_ -> Raster.get 0 -1 raster |> Expect.equal Nothing)
            , Test.test "out of bounds right" (\_ -> Raster.get 5 0 raster |> Expect.equal Nothing)
            , Test.test "out of bounds bottom" (\_ -> Raster.get 0 10 raster |> Expect.equal Nothing)
            ]
        , Test.describe "set" <|
            let
                raster =
                    Raster
                        { size =
                            { width = 12
                            , height = 10
                            }
                        , quadSizes = [ 8, 4, 2, 1 ]
                        , root = LeafNode A
                        }
            in
            [ Test.test "same value leaf"
                (\_ ->
                    raster
                        |> Raster.set 0 0 A
                        |> getRoot
                        |> Expect.equal (LeafNode A)
                )
            , Test.test "full branching all quads"
                (\_ ->
                    raster
                        |> Raster.set 5 3 B
                        |> getRoot
                        |> Expect.equal
                            (BranchNode
                                { q1 =
                                    BranchNode
                                        { q1 = LeafNode A
                                        , q2 =
                                            BranchNode
                                                { q1 = LeafNode A
                                                , q2 = LeafNode A
                                                , q3 =
                                                    BranchNode
                                                        { q1 = LeafNode A
                                                        , q2 = LeafNode A
                                                        , q3 = LeafNode A
                                                        , q4 = LeafNode B
                                                        }
                                                , q4 = LeafNode A
                                                }
                                        , q3 = LeafNode A
                                        , q4 = LeafNode A
                                        }
                                , q2 = LeafNode A
                                , q3 = LeafNode A
                                , q4 = LeafNode A
                                }
                            )
                )
            , Test.test "collapse to root all quads"
                (\_ ->
                    raster
                        |> Raster.set 5 3 B
                        |> Raster.set 5 3 A
                        |> getRoot
                        |> Expect.equal (LeafNode A)
                )
            , Test.test "out of bounds left"
                (\_ ->
                    raster
                        |> Raster.set -1 0 C
                        |> getRoot
                        |> Expect.equal (LeafNode A)
                )
            , Test.test "out of bounds top"
                (\_ ->
                    raster
                        |> Raster.set 0 -1 C
                        |> getRoot
                        |> Expect.equal (LeafNode A)
                )
            , Test.test "out of bounds right"
                (\_ ->
                    raster
                        |> Raster.set 12 0 C
                        |> getRoot
                        |> Expect.equal (LeafNode A)
                )
            , Test.test "out of bounds bottom"
                (\_ ->
                    raster
                        |> Raster.set 0 10 C
                        |> getRoot
                        |> Expect.equal (LeafNode A)
                )
            ]
        , Test.describe "serialization"
            [ Test.test "serialize" <|
                \_ ->
                    Raster.init (Size 12 10) A
                        |> Raster.set 5 3 B
                        |> Raster.serialize
                        |> Expect.equal
                            [ Leaf A
                            , Leaf A
                            , Leaf A
                            , Leaf A
                            , Leaf A
                            , Leaf A
                            , Leaf B
                            , Leaf A
                            , Leaf A
                            , Leaf A
                            , Branch
                            , Leaf A
                            , Leaf A
                            , Branch
                            , Leaf A
                            , Branch
                            , Branch
                            ]
            , Test.test "deserialize" <|
                \_ ->
                    [ Leaf A
                    , Leaf A
                    , Leaf A
                    , Leaf A
                    , Leaf A
                    , Leaf A
                    , Leaf B
                    , Leaf A
                    , Leaf A
                    , Leaf A
                    , Branch
                    , Leaf A
                    , Leaf A
                    , Branch
                    , Leaf A
                    , Branch
                    , Branch
                    ]
                        |> Raster.deserialize (Size 12 10)
                        |> Expect.equal
                            (Just
                                (Raster.init (Size 12 10) A
                                    |> Raster.set 5 3 B
                                )
                            )
            ]
        , Test.fuzz rasterValuesFuzzer "set/get fuzz values" <|
            \( size, initialValue, values ) ->
                Raster.init size initialValue
                    |> setRasterValues values
                    |> Expect.all
                        (values
                            |> Dict.fromList
                            |> Dict.toList
                            |> List.map
                                (\( ( x, y ), value ) ->
                                    Raster.get x y
                                        >> Expect.equal
                                            (if x < 0 || y < 0 || x >= size.width || y >= size.height then
                                                Nothing

                                             else
                                                Just value
                                            )
                                )
                        )
        , Test.fuzz rasterValuesFuzzer "serialize/deserialize fuzz values" <|
            \( size, initialValue, values ) ->
                let
                    raster =
                        Raster.init size initialValue
                            |> setRasterValues values
                in
                raster
                    |> Raster.serialize
                    |> Raster.deserialize size
                    |> Maybe.map (Expect.equal raster)
                    |> Maybe.withDefault (Expect.fail "deserialize failed")
        ]


isPowerOfTwo : Int -> Bool
isPowerOfTwo n =
    n > 0 && (Bitwise.and n (n - 1) == 0)


setRasterValues : List ( ( Int, Int ), Value ) -> Raster Value -> Raster Value
setRasterValues values raster =
    List.foldl (\( ( x, y ), value ) -> Raster.set x y value) raster values


rasterValuesFuzzer : Fuzz.Fuzzer ( Size, Value, List ( ( Int, Int ), Value ) )
rasterValuesFuzzer =
    Fuzz.tuple3
        ( Fuzz.map2 Size Fuzz.int Fuzz.int
        , valueFuzzer
        , Fuzz.map2 (\point list -> point :: list) rasterValue (Fuzz.list rasterValue)
        )


rasterValue : Fuzz.Fuzzer ( ( Int, Int ), Value )
rasterValue =
    Fuzz.tuple
        ( Fuzz.tuple ( Fuzz.int, Fuzz.int )
        , valueFuzzer
        )


valueFuzzer : Fuzz.Fuzzer Value
valueFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant A
        , Fuzz.constant B
        , Fuzz.constant C
        , Fuzz.constant D
        ]
