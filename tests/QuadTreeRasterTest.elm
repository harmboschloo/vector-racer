module QuadTreeRasterTest exposing (suite)

import Expect
import QuadTreeRaster.Internal as Internal exposing (Node(..), Size)
import Test exposing (Test)


type Value
    = A
    | B
    | C
    | D


suite : Test
suite =
    Test.describe "QuadTreeRaster"
        [ Test.test "init" <|
            \_ ->
                Internal.init (Size 5 10) A
                    |> Expect.equal
                        { config =
                            { width = 5
                            , height = 10
                            , quadSize = 8
                            }
                        , values = Leaf A
                        }
        , Test.describe "get" <|
            let
                model =
                    { config =
                        { width = 5
                        , height = 10
                        , quadSize = 8
                        }
                    , values =
                        Branch
                            { q1 = Leaf A
                            , q2 = Leaf B
                            , q3 = Leaf C
                            , q4 = Leaf D
                            }
                    }
            in
            [ Test.test "q1 edge 0 0" (\_ -> Internal.get 0 0 model |> Expect.equal (Just A))
            , Test.test "q1 edge 1 0" (\_ -> Internal.get 4 0 model |> Expect.equal (Just A))
            , Test.test "q1 edge 0 1" (\_ -> Internal.get 0 7 model |> Expect.equal (Just A))
            , Test.test "q1 edge 1 1" (\_ -> Internal.get 4 7 model |> Expect.equal (Just A))
            , Test.test "q3 edge 0 0" (\_ -> Internal.get 0 8 model |> Expect.equal (Just C))
            , Test.test "q3 edge 1 0" (\_ -> Internal.get 4 8 model |> Expect.equal (Just C))
            , Test.test "q3 edge 0 1" (\_ -> Internal.get 0 9 model |> Expect.equal (Just C))
            , Test.test "q3 edge 1 1" (\_ -> Internal.get 4 9 model |> Expect.equal (Just C))
            , Test.test "out of bounds left" (\_ -> Internal.get -1 0 model |> Expect.equal Nothing)
            , Test.test "out of bounds top" (\_ -> Internal.get 0 -1 model |> Expect.equal Nothing)
            , Test.test "out of bounds right" (\_ -> Internal.get 5 0 model |> Expect.equal Nothing)
            , Test.test "out of bounds bottom" (\_ -> Internal.get 0 10 model |> Expect.equal Nothing)
            ]
        , Test.describe "set" <|
            let
                model =
                    { config =
                        { width = 12
                        , height = 10
                        , quadSize = 8
                        }
                    , values = Leaf A
                    }
            in
            [ Test.test "same value leaf"
                (\_ ->
                    model
                        |> Internal.set 0 0 A
                        |> .values
                        |> Expect.equal (Leaf A)
                )
            , Test.test "full branching all quads"
                (\_ ->
                    model
                        |> Internal.set 5 3 B
                        |> .values
                        |> Expect.equal
                            (Branch
                                { q1 =
                                    Branch
                                        { q1 = Leaf A
                                        , q2 =
                                            Branch
                                                { q1 = Leaf A
                                                , q2 = Leaf A
                                                , q3 =
                                                    Branch
                                                        { q1 = Leaf A
                                                        , q2 = Leaf A
                                                        , q3 = Leaf A
                                                        , q4 = Leaf B
                                                        }
                                                , q4 = Leaf A
                                                }
                                        , q3 = Leaf A
                                        , q4 = Leaf A
                                        }
                                , q2 = Leaf A
                                , q3 = Leaf A
                                , q4 = Leaf A
                                }
                            )
                )
            , Test.test "collapse to root all quads"
                (\_ ->
                    model
                        |> Internal.set 5 3 B
                        |> Internal.set 5 3 A
                        |> .values
                        |> Expect.equal (Leaf A)
                )
            , Test.test "out of bounds left"
                (\_ ->
                    model
                        |> Internal.set -1 0 C
                        |> .values
                        |> Expect.equal (Leaf A)
                )
            , Test.test "out of bounds top"
                (\_ ->
                    model
                        |> Internal.set 0 -1 C
                        |> .values
                        |> Expect.equal (Leaf A)
                )
            , Test.test "out of bounds right"
                (\_ ->
                    model
                        |> Internal.set 12 0 C
                        |> .values
                        |> Expect.equal (Leaf A)
                )
            , Test.test "out of bounds bottom"
                (\_ ->
                    model
                        |> Internal.set 0 10 C
                        |> .values
                        |> Expect.equal (Leaf A)
                )
            ]
        ]
