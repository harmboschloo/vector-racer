module VectorRacer exposing
    ( Acceleration
    , Position
    , Velocity
    , encodePosition
    , positionDecoder
    )

import Json.Decode
import Json.Encode
import VectorRacer.Pixels as Pixels exposing (Pixels)
import VectorRacer.Vector exposing (Vector)


type alias Position =
    Vector Int Pixels


type alias Velocity =
    Vector Int Pixels.PixelsPerStep


type alias Acceleration =
    Vector Int Pixels.PixelsPerStepSquared



-- JSON --


encodePosition : Position -> Json.Encode.Value
encodePosition =
    Pixels.encodePixels Json.Encode.int


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Pixels.pixelsDecoder Json.Decode.int
