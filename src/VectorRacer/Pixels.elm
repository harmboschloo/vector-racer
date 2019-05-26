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
import VectorRacer.Steps exposing (Steps)
import VectorRacer.Vector as Vector exposing (Vector)


type alias Pixels =
    Pixels.Pixels


type alias PixelsPerStep =
    Quantity.Rate Pixels Steps


type alias PixelsPerStepSquared =
    Quantity.Rate PixelsPerStep Steps


pixels : ( number, number ) -> Vector number Pixels
pixels =
    Vector.fromComponents Quantity


inPixels : Vector number Pixels -> ( number, number )
inPixels =
    Vector.toComponents unwrapQuantity


pixelsPerStep : ( number, number ) -> Vector number PixelsPerStep
pixelsPerStep =
    Vector.fromComponents Quantity


inPixelsPerStep : Vector number PixelsPerStep -> ( number, number )
inPixelsPerStep =
    Vector.toComponents unwrapQuantity


pixelsPerStepSquared : ( number, number ) -> Vector number PixelsPerStepSquared
pixelsPerStepSquared =
    Vector.fromComponents Quantity


inPixelsPerStepSquared : Vector number PixelsPerStepSquared -> ( number, number )
inPixelsPerStepSquared =
    Vector.toComponents unwrapQuantity


unwrapQuantity : Quantity number units -> number
unwrapQuantity (Quantity a) =
    a



-- JSON --


encodePixels : (number -> Json.Encode.Value) -> Vector number Pixels -> Json.Encode.Value
encodePixels encodeValue =
    Vector.encode (unwrapQuantity >> encodeValue)


pixelsDecoder : Json.Decode.Decoder number -> Json.Decode.Decoder (Vector number Pixels)
pixelsDecoder valueDecoder =
    Vector.decoder (Json.Decode.map Quantity valueDecoder)
