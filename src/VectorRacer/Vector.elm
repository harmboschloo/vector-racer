module VectorRacer.Vector exposing
    ( Position, Velocity, Acceleration, Size, Padding
    , Vector, init, x, y
    , xToPx, yToPx, toTupleString
    , encode, decoder
    )

{-|

@docs Position, Velocity, Acceleration, Size, Padding
@docs Vector, init, x, y
@docs xToPx, yToPx, toTupleString
@docs encode, decoder

-}

import Json.Decode
import Json.Encode



-- TYPES --


type alias Position =
    Vector Pos


type Pos
    = Pos


type alias Velocity =
    Vector Vel


type Vel
    = Vel


type alias Acceleration =
    Vector Acc


type Acc
    = Acc


type alias Size =
    Vector Siz


type Siz
    = Siz


type alias Padding =
    Vector Pad


type Pad
    = Pad



-- VECTOR --


type Vector a
    = Vector
        { x : Int
        , y : Int
        }


init : Int -> Int -> Vector a
init xValue yValue =
    Vector
        { x = xValue
        , y = yValue
        }


x : Vector a -> Int
x (Vector vector) =
    vector.x


y : Vector a -> Int
y (Vector vector) =
    vector.y



-- STRINGS --


xToPx : Vector a -> String
xToPx (Vector vector) =
    String.fromInt vector.x ++ "px"


yToPx : Vector a -> String
yToPx (Vector vector) =
    String.fromInt vector.y ++ "px"


toTupleString : Vector a -> String
toTupleString (Vector vector) =
    "(" ++ String.fromInt vector.x ++ "," ++ String.fromInt vector.y ++ ")"



-- JSON --


encode : Vector a -> Json.Encode.Value
encode (Vector vector) =
    Json.Encode.object
        [ ( "x", Json.Encode.int vector.x )
        , ( "y", Json.Encode.int vector.y )
        ]


decoder : Json.Decode.Decoder (Vector a)
decoder =
    Json.Decode.map2 init
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)
