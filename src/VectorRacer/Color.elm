module VectorRacer.Color exposing (Color, bytesDecoder, bytesEncoder)

import Bytes.Decode
import Bytes.Encode


type alias Color =
    { r : Int
    , g : Int
    , b : Int
    , a : Int
    }


bytesDecoder : Bytes.Decode.Decoder Color
bytesDecoder =
    Bytes.Decode.map4 Color
        Bytes.Decode.unsignedInt8
        Bytes.Decode.unsignedInt8
        Bytes.Decode.unsignedInt8
        Bytes.Decode.unsignedInt8


bytesEncoder : Color -> Bytes.Encode.Encoder
bytesEncoder { r, g, b, a } =
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt8 r
        , Bytes.Encode.unsignedInt8 g
        , Bytes.Encode.unsignedInt8 b
        , Bytes.Encode.unsignedInt8 a
        ]
