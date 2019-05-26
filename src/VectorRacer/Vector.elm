module VectorRacer.Vector exposing
    ( Vector
    , decoder
    , distance
    , divideByInt
    , encode
    , fromComponents
    , fromFloats
    , fromInts
    , fromQuantities
    , mean
    , minus
    , multiplyBy
    , plus
    , toComponents
    , toFloatVector
    , toFloats
    , toInts
    , toQuantities
    )

{-| -}

import Json.Decode
import Json.Encode
import Quantity exposing (Quantity(..))



-- MODEL --


type Vector number units
    = Vector
        { x : Quantity number units
        , y : Quantity number units
        }


fromQuantities : ( Quantity number units, Quantity number units ) -> Vector number units
fromQuantities ( x, y ) =
    Vector
        { x = x
        , y = y
        }


toQuantities : Vector number units -> ( Quantity number units, Quantity number units )
toQuantities (Vector { x, y }) =
    ( x, y )


fromComponents : (number -> Quantity number units) -> ( number, number ) -> Vector number units
fromComponents fn ( x, y ) =
    fromQuantities ( fn x, fn y )


toComponents : (Quantity number units -> number) -> Vector number units -> ( number, number )
toComponents fn (Vector { x, y }) =
    ( fn x, fn y )


fromFloats : ( Float, Float ) -> Vector Float Quantity.Unitless
fromFloats =
    fromComponents Quantity.float


toFloats : Vector Float Quantity.Unitless -> ( Float, Float )
toFloats =
    toComponents Quantity.toFloat


fromInts : ( Int, Int ) -> Vector Int Quantity.Unitless
fromInts =
    fromComponents Quantity.int


toInts : Vector Int Quantity.Unitless -> ( Int, Int )
toInts =
    toComponents Quantity.toInt



-- MAPS --


map : (Quantity number1 units1 -> Quantity number2 units2) -> Vector number1 units1 -> Vector number2 units2
map fn (Vector a) =
    Vector
        { x = fn a.x
        , y = fn a.y
        }


map2 :
    (Quantity number1 units1 -> Quantity number2 units2 -> Quantity number3 units3)
    -> Vector number1 units1
    -> Vector number2 units2
    -> Vector number3 units3
map2 fn (Vector a) (Vector b) =
    Vector
        { x = fn a.x b.x
        , y = fn a.y b.y
        }



-- ARITHMETIC --


plus : Vector number units -> Vector number units -> Vector number units
plus =
    map2 Quantity.plus


minus : Vector number units -> Vector number units -> Vector number units
minus =
    map2 Quantity.minus


divideBy : Vector Float Quantity.Unitless -> Vector Float units -> Vector Float units
divideBy =
    map2 (Quantity.toFloat >> Quantity.divideBy)


multiplyBy : Vector Float Quantity.Unitless -> Vector Float units -> Vector Float units
multiplyBy =
    map2 (Quantity.toFloat >> Quantity.multiplyBy)


divideByInt : Vector Int Quantity.Unitless -> Vector Int units -> Vector Int units
divideByInt b a =
    map2 divideQuantityByInt b a


divideQuantityByInt : Quantity Int Quantity.Unitless -> Quantity Int units -> Quantity Int units
divideQuantityByInt (Quantity b) (Quantity a) =
    Quantity (a // b)


distance : Vector Float units -> Vector Float units -> Quantity Float units
distance (Vector a) (Vector b) =
    let
        (Quantity x1) =
            a.x

        (Quantity y1) =
            a.y

        (Quantity x2) =
            b.x

        (Quantity y2) =
            b.y
    in
    Quantity (sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))


mean : Vector Float units -> Vector Float units -> Vector Float units
mean a b =
    a |> plus b |> divideBy (fromFloats ( 2, 2 ))



-- INTS / FLOATS --


toFloatVector : Vector Int units -> Vector Float units
toFloatVector =
    map Quantity.toFloatQuantity



-- JSON --


encode : (Quantity number units -> Json.Encode.Value) -> Vector number units -> Json.Encode.Value
encode encodeQuantity (Vector vector) =
    Json.Encode.object
        [ ( "x", encodeQuantity vector.x )
        , ( "y", encodeQuantity vector.y )
        ]


decoder : Json.Decode.Decoder (Quantity number units) -> Json.Decode.Decoder (Vector number units)
decoder quantityDecoder =
    Json.Decode.map2 (\x y -> fromQuantities ( x, y ))
        (Json.Decode.field "x" quantityDecoder)
        (Json.Decode.field "y" quantityDecoder)
