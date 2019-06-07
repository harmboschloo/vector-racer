port module Main exposing (main)

import Base64
import Bytes.Encode
import Json.Decode
import Json.Encode
import VectorRacer.Track as Track
import VectorRacer.Track.Image1 as Image1
import VectorRacer.Track.ImageHelpers as ImageHelpers



-- INIT --


type alias Model =
    ()


init : Json.Decode.Value -> ( Model, Cmd Msg )
init trackJson =
    case Json.Decode.decodeValue Track.decoder trackJson of
        Ok (Ok track) ->
            case
                track
                    |> Track.getSurfaces
                    |> Image1.fromSurfaces
                    |> ImageHelpers.bytesEncoder
                    |> Bytes.Encode.encode
                    |> Base64.fromBytes
            of
                Just bytesString ->
                    ( (), bytesString |> Json.Encode.string |> onImage )

                Nothing ->
                    ( (), "Base64.fromBytes error" |> Json.Encode.string |> onError )

        Ok (Err error) ->
            ( (), error |> Track.decodeErrorToString |> Json.Encode.string |> onError )

        Err error ->
            ( (), error |> Json.Decode.errorToString |> Json.Encode.string |> onError )



-- UPDATE --


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- PORTS --


port onImage : Json.Encode.Value -> Cmd msg


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
