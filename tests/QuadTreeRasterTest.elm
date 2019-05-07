module QuadTreeRasterTest exposing (suite)

import Expect
import QuadTreeRaster as Raster exposing (Size, Token(..))
import QuadTreeRaster.Internal exposing (Model, Node(..), Raster(..))
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
                            , quadSize = 8
                            , root = LeafNode A
                            }
                        )
        , Test.test "getSize" <|
            \_ ->
                Raster.init (Size 5 10) A
                    |> Raster.getSize
                    |> Expect.equal
                        { width = 5
                        , height = 10
                        }
        , Test.describe "get" <|
            let
                raster =
                    Raster
                        { size =
                            { width = 5
                            , height = 10
                            }
                        , quadSize = 8
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
                        , quadSize = 8
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
        , Test.test "serialize" <|
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
