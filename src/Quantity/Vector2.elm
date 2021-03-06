module Quantity.Vector2 exposing
    ( Vector2
    , ceiling
    , decoder
    , distance
    , divideBy
    , divideByInt
    , encode
    , floor
    , fromComponents
    , fromFloat
    , fromFloats
    , fromInt
    , fromInts
    , fromQuantities
    , fromQuantity
    , map
    , map2
    , map3
    , map4
    , midpoint
    , minus
    , multiplyBy
    , plus
    , round
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


type Vector2 number units
    = Vector2
        { x : Quantity number units
        , y : Quantity number units
        }


fromQuantities : ( Quantity number units, Quantity number units ) -> Vector2 number units
fromQuantities ( x, y ) =
    Vector2
        { x = x
        , y = y
        }


fromQuantity : Quantity number units -> Vector2 number units
fromQuantity value =
    Vector2
        { x = value
        , y = value
        }


toQuantities : Vector2 number units -> ( Quantity number units, Quantity number units )
toQuantities (Vector2 { x, y }) =
    ( x, y )


fromComponents : (number -> Quantity number units) -> ( number, number ) -> Vector2 number units
fromComponents fn ( x, y ) =
    fromQuantities ( fn x, fn y )


toComponents : (Quantity number units -> number) -> Vector2 number units -> ( number, number )
toComponents fn (Vector2 { x, y }) =
    ( fn x, fn y )


fromFloats : ( Float, Float ) -> Vector2 Float Quantity.Unitless
fromFloats =
    fromComponents Quantity.float


toFloats : Vector2 Float Quantity.Unitless -> ( Float, Float )
toFloats =
    toComponents Quantity.toFloat


fromFloat : Float -> Vector2 Float Quantity.Unitless
fromFloat value =
    fromComponents Quantity.float ( value, value )


fromInts : ( Int, Int ) -> Vector2 Int Quantity.Unitless
fromInts =
    fromComponents Quantity.int


toInts : Vector2 Int Quantity.Unitless -> ( Int, Int )
toInts =
    toComponents Quantity.toInt


fromInt : Int -> Vector2 Int Quantity.Unitless
fromInt value =
    fromComponents Quantity.int ( value, value )



-- MAPS --


map : (Quantity number1 units1 -> Quantity number2 units2) -> Vector2 number1 units1 -> Vector2 number2 units2
map fn (Vector2 a) =
    Vector2
        { x = fn a.x
        , y = fn a.y
        }


map2 :
    (Quantity number1 units1 -> Quantity number2 units2 -> Quantity number3 units3)
    -> Vector2 number1 units1
    -> Vector2 number2 units2
    -> Vector2 number3 units3
map2 fn (Vector2 a) (Vector2 b) =
    Vector2
        { x = fn a.x b.x
        , y = fn a.y b.y
        }


map3 :
    (Quantity number1 units1 -> Quantity number2 units2 -> Quantity number3 units3 -> Quantity number4 units4)
    -> Vector2 number1 units1
    -> Vector2 number2 units2
    -> Vector2 number3 units3
    -> Vector2 number4 units4
map3 fn (Vector2 a) (Vector2 b) (Vector2 c) =
    Vector2
        { x = fn a.x b.x c.x
        , y = fn a.y b.y c.y
        }


map4 :
    (Quantity number1 units1
     -> Quantity number2 units2
     -> Quantity number3 units3
     -> Quantity number4 units4
     -> Quantity number5 units5
    )
    -> Vector2 number1 units1
    -> Vector2 number2 units2
    -> Vector2 number3 units3
    -> Vector2 number4 units4
    -> Vector2 number5 units5
map4 fn (Vector2 a) (Vector2 b) (Vector2 c) (Vector2 d) =
    Vector2
        { x = fn a.x b.x c.x d.x
        , y = fn a.y b.y c.y d.y
        }



-- ARITHMETIC --


plus : Vector2 number units -> Vector2 number units -> Vector2 number units
plus =
    map2 Quantity.plus


minus : Vector2 number units -> Vector2 number units -> Vector2 number units
minus =
    map2 Quantity.minus


divideBy : Vector2 Float Quantity.Unitless -> Vector2 Float units -> Vector2 Float units
divideBy =
    map2 (Quantity.toFloat >> Quantity.divideBy)


multiplyBy : Vector2 Float Quantity.Unitless -> Vector2 Float units -> Vector2 Float units
multiplyBy =
    map2 (Quantity.toFloat >> Quantity.multiplyBy)


divideByInt : Vector2 Int Quantity.Unitless -> Vector2 Int units -> Vector2 Int units
divideByInt b a =
    map2 divideQuantityByInt b a


divideQuantityByInt : Quantity Int Quantity.Unitless -> Quantity Int units -> Quantity Int units
divideQuantityByInt (Quantity b) (Quantity a) =
    Quantity (a // b)


distance : Vector2 Float units -> Vector2 Float units -> Quantity Float units
distance (Vector2 a) (Vector2 b) =
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


midpoint : Vector2 Float units -> Vector2 Float units -> Vector2 Float units
midpoint =
    map2 Quantity.midpoint


round : Vector2 Float units -> Vector2 Int units
round =
    map Quantity.round


floor : Vector2 Float units -> Vector2 Int units
floor =
    map Quantity.floor


ceiling : Vector2 Float units -> Vector2 Int units
ceiling =
    map Quantity.ceiling



-- INTS / FLOATS --


toFloatVector : Vector2 Int units -> Vector2 Float units
toFloatVector =
    map Quantity.toFloatQuantity



-- JSON --


encode : (Quantity number units -> Json.Encode.Value) -> Vector2 number units -> Json.Encode.Value
encode encodeQuantity (Vector2 vector) =
    Json.Encode.object
        [ ( "x", encodeQuantity vector.x )
        , ( "y", encodeQuantity vector.y )
        ]


decoder : Json.Decode.Decoder (Quantity number units) -> Json.Decode.Decoder (Vector2 number units)
decoder quantityDecoder =
    Json.Decode.map2 (\x y -> fromQuantities ( x, y ))
        (Json.Decode.field "x" quantityDecoder)
        (Json.Decode.field "y" quantityDecoder)
