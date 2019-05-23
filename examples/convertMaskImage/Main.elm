port module Main exposing (main)

import Bytes exposing (Bytes)
import File exposing (File)
import Json.Decode
import Json.Encode
import Task
import VectorRacer.Track as Track exposing (Track)
import VectorRacer.Vector as Vector exposing (SizeVector)



-- INIT --


type alias Flags =
    Json.Decode.Value


type alias Model =
    ()


type alias ImageDataFile =
    { width : Int
    , height : Int
    , bytesFile : File
    }


imageDataFileDecoder : Json.Decode.Decoder ImageDataFile
imageDataFileDecoder =
    Json.Decode.map3 ImageDataFile
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "bytesFile" File.decoder)


init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue imageDataFileDecoder flags of
        Ok file ->
            ( ()
            , Task.perform
                (ImageBytesLoaded (Vector.init file.width file.height))
                (File.toBytes file.bytesFile)
            )

        Err error ->
            ( ()
            , onError (error |> Json.Decode.errorToString |> Json.Encode.string)
            )



-- UPDATE --


type Msg
    = ImageBytesLoaded SizeVector Bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ImageBytesLoaded size bytes ->
            case Track.fromMaskBytes size bytes of
                Ok track ->
                    ( model
                    , onTrack (Track.encode track)
                    )

                Err error ->
                    ( model
                    , onError (error |> Debug.toString |> Json.Encode.string)
                    )



-- PORTS --


port onTrack : Json.Encode.Value -> Cmd msg


port onError : Json.Encode.Value -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN --


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
