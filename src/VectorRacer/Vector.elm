module VectorRacer.Vector exposing
    ( PositionVector, VelocityVector, AccelerationVector, SizeVector, OffsetVector, ScaleVector
    , Position, Velocity, Acceleration, Size, Offset, Scale
    , Vector, init, x, y
    , divideBy
    , xToPx, yToPx, toTupleString
    , encode, decoder
    )

{-|

@docs PositionVector, VelocityVector, AccelerationVector, SizeVector, OffsetVector, ScaleVector
@docs Position, Velocity, Acceleration, Size, Offset, Scale
@docs Vector, init, x, y
@docs divideBy
@docs xToPx, yToPx, toTupleString
@docs encode, decoder

-}

import Json.Decode
import Json.Encode



-- TYPES --


type alias PositionVector =
    Vector Position


type Position
    = Position


type alias VelocityVector =
    Vector Velocity


type Velocity
    = Velocity


type alias AccelerationVector =
    Vector Acceleration


type Acceleration
    = Acceleration


type alias SizeVector =
    Vector Size


type Size
    = Size


type alias OffsetVector =
    Vector Offset


type Offset
    = Offset


type alias ScaleVector =
    Vector Scale


type Scale
    = Scale



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



-- MATH --


divideBy : Vector a -> Vector b -> Vector c
divideBy (Vector b) (Vector a) =
    Vector
        { x = a.x // b.x
        , y = a.y // b.y
        }



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
