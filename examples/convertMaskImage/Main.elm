port module Main exposing (main)

import Base64
import Json.Decode
import Json.Encode
import VectorRacer.Track as Track exposing (Track)
import VectorRacer.Vector.Pixels as Pixels



-- INIT --


type alias Model =
    ()


type alias Flags =
    { width : Int
    , height : Int
    , bytesString : String
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    decodeFlags flags
        |> Result.andThen decodeTrack
        |> resultCmd
        |> Tuple.pair ()


decodeFlags : Json.Decode.Value -> Result String Flags
decodeFlags =
    Json.Decode.decodeValue flagsDecoder >> Result.mapError Json.Decode.errorToString


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map3 Flags
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "bytesString" Json.Decode.string)


decodeTrack : Flags -> Result String Track
decodeTrack { width, height, bytesString } =
    Base64.toBytes bytesString
        |> Maybe.map
            (Track.fromMaskBytes (Pixels.pixels ( width, height ))
                >> Result.mapError Track.maskBytesErrorToString
            )
        |> Maybe.withDefault (Err "Base64.toBytes failed")


resultCmd : Result String Track -> Cmd Msg
resultCmd result =
    case result of
        Ok track ->
            onTrack (Track.encode track)

        Err error ->
            onError (Json.Encode.string error)



-- UPDATE --


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- PORTS --


port onTrack : Json.Encode.Value -> Cmd msg


port onError : Json.Encode.Value -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN --


main : Program Json.Decode.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
