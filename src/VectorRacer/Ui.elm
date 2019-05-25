module VectorRacer.Ui exposing
    ( Offset
    , Scale
    , Size
    , toTupleString
    , xToPx
    , xToString
    , yToPx
    , yToString
    )

import Quantity
import VectorRacer.Pixels as Pixels exposing (Pixels)
import VectorRacer.Vector exposing (Vector)


type alias Size =
    Vector Int Pixels


type alias Offset =
    Vector Float Pixels


type alias Scale =
    Vector Float Quantity.Unitless


xToPx : Vector Int Pixels -> String
xToPx vector =
    xToString vector ++ "px"


xToString : Vector Int Pixels -> String
xToString vector =
    vector |> Pixels.inPixels |> Tuple.first |> String.fromInt


yToPx : Vector Int Pixels -> String
yToPx vector =
    yToString vector ++ "px"


yToString : Vector Int Pixels -> String
yToString vector =
    vector |> Pixels.inPixels |> Tuple.second |> String.fromInt


toTupleString : Vector Float Pixels -> String
toTupleString vector =
    let
        ( x, y ) =
            Pixels.inPixels vector
    in
    "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
