module VectorRacer.Pixels exposing
    ( Pixels
    , PixelsPerStep
    , PixelsPerStepSquared
    , encodePixels
    , inPixels
    , inPixelsPerStep
    , inPixelsPerStepSquared
    , pixels
    , pixelsDecoder
    , pixelsPerStep
    , pixelsPerStepSquared
    )

import Json.Decode
import Json.Encode
import Pixels
import Quantity exposing (Quantity(..))
import Quantity.Vector2 as Vector2 exposing (Vector2)
import VectorRacer.Steps exposing (Steps)


type alias Pixels =
    Pixels.Pixels


type alias PixelsPerStep =
    Quantity.Rate Pixels Steps


type alias PixelsPerStepSquared =
    Quantity.Rate PixelsPerStep Steps


pixels : ( number, number ) -> Vector2 number Pixels
pixels =
    Vector2.fromComponents Quantity


inPixels : Vector2 number Pixels -> ( number, number )
inPixels =
    Vector2.toComponents unwrapQuantity


pixelsPerStep : ( number, number ) -> Vector2 number PixelsPerStep
pixelsPerStep =
    Vector2.fromComponents Quantity


inPixelsPerStep : Vector2 number PixelsPerStep -> ( number, number )
inPixelsPerStep =
    Vector2.toComponents unwrapQuantity


pixelsPerStepSquared : ( number, number ) -> Vector2 number PixelsPerStepSquared
pixelsPerStepSquared =
    Vector2.fromComponents Quantity


inPixelsPerStepSquared : Vector2 number PixelsPerStepSquared -> ( number, number )
inPixelsPerStepSquared =
    Vector2.toComponents unwrapQuantity


unwrapQuantity : Quantity number units -> number
unwrapQuantity (Quantity a) =
    a



-- JSON --


encodePixels : (number -> Json.Encode.Value) -> Vector2 number Pixels -> Json.Encode.Value
encodePixels encodeValue =
    Vector2.encode (unwrapQuantity >> encodeValue)


pixelsDecoder : Json.Decode.Decoder number -> Json.Decode.Decoder (Vector2 number Pixels)
pixelsDecoder valueDecoder =
    Vector2.decoder (Json.Decode.map Quantity valueDecoder)
