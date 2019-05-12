module VectorRacer.Vector exposing
    ( Acceleration
    , Position
    , Velocity
    , decoder
    , encode
    , init
    , toString
    , x
    , y
    )

import Json.Decode
import Json.Encode


type alias Position =
    Vector Pos


type alias Velocity =
    Vector Vel


type alias Acceleration =
    Vector Acc


type Pos
    = Pos


type Vel
    = Vel


type Acc
    = Acc


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


toString : Vector a -> String
toString (Vector vector) =
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
