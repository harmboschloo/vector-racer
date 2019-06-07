port module Main exposing (main)

import Base64
import Json.Decode
import Json.Encode
import Task
import Timing
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


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map3 Flags
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "bytesString" Json.Decode.string)


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue flagsDecoder flags of
        Ok { width, height, bytesString } ->
            Task.succeed ( bytesString, [] )
                |> Timing.andTime "Base64.toBytes" Base64.toBytes
                |> Timing.andThen (Timing.maybeToTask "Base64.toBytes failed")
                |> Timing.andTime "Track.fromMaskBytes" (Track.fromMaskBytes (Pixels.pixels ( width, height )))
                |> Timing.map (Result.mapError Track.maskBytesErrorToString)
                |> Timing.andThen Timing.resultToTask
                |> Task.attempt GotResult
                |> Tuple.pair ()

        Err error ->
            ( (), error |> Json.Decode.errorToString |> Json.Encode.string |> onError )



-- UPDATE --


type Msg
    = GotResult (Result String ( Track, List ( String, Int ) ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResult result ->
            ( model
            , case result of
                Ok ( track, timings ) ->
                    Cmd.batch
                        [ onTrack (Track.encode track)
                        , onTimings (Timing.encodeList timings)
                        ]

                Err error ->
                    onError (Json.Encode.string error)
            )



-- PORTS --


port onTrack : Json.Encode.Value -> Cmd msg


port onTimings : Json.Encode.Value -> Cmd msg


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
