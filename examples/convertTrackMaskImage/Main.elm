port module Main exposing (main)

import Json.Decode
import Json.Encode
import Task
import Timing
import VectorRacer.Pixels as Pixels
import VectorRacer.Track as Track exposing (Track)



-- INIT --


type alias Model =
    ()


type alias Flags =
    { width : Int
    , height : Int
    , bytesList : List Int
    }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map3 Flags
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "bytesArray" (Json.Decode.list Json.Decode.int))


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    Task.succeed ( flags, [] )
        |> Timing.andTime "decode flags" (Json.Decode.decodeValue flagsDecoder)
        |> Timing.map (Result.mapError Json.Decode.errorToString)
        |> Timing.andThenMap Timing.resultToTask
        |> Timing.andTime "Track.fromMaskBytesList"
            (\{ width, height, bytesList } -> Track.fromMaskBytesList (Pixels.pixels ( width, height )) bytesList)
        |> Timing.map (Result.mapError Track.maskBytesErrorToString)
        |> Timing.andThenMap Timing.resultToTask
        |> Task.attempt GotResult
        |> Tuple.pair ()



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
