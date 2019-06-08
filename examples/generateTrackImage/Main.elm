port module Main exposing (main)

import Json.Decode
import Json.Encode
import Task
import Timing
import VectorRacer.Track as Track
import VectorRacer.Track.Image1 as Image1
import VectorRacer.Track.ImageHelpers as ImageHelpers



-- INIT --


type alias Model =
    ()


init : Json.Decode.Value -> ( Model, Cmd Msg )
init trackJson =
    Task.succeed ( trackJson, [] )
        |> Timing.andTime "decode track" (Json.Decode.decodeValue Track.decoder)
        |> Timing.map (Result.mapError Json.Decode.errorToString)
        |> Timing.andThen Timing.resultToTask
        |> Timing.map (Result.mapError Track.decodeErrorToString)
        |> Timing.andThen Timing.resultToTask
        |> Timing.map Track.getSurfaces
        |> Timing.andTime "Image1.fromSurfaces" Image1.fromSurfaces
        |> Timing.andTime "ImageHelpers.toBytesList" ImageHelpers.toBytesList
        |> Timing.andTime "Json.Encode.list" (Json.Encode.list Json.Encode.int)
        |> Task.attempt GotResult
        |> Tuple.pair ()



-- UPDATE --


type Msg
    = GotResult (Result String ( Json.Encode.Value, List ( String, Int ) ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResult result ->
            ( model
            , case result of
                Ok ( image, timings ) ->
                    Cmd.batch
                        [ onImage image
                        , onTimings (Timing.encodeList timings)
                        ]

                Err error ->
                    onError (Json.Encode.string error)
            )



-- PORTS --


port onImage : Json.Encode.Value -> Cmd msg


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
