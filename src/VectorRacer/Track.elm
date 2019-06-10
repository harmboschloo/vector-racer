module VectorRacer.Track exposing
    ( Track, Surface(..), Checkpoint(..), Size, Position, Velocity, Acceleration
    , getSize, getSurface, getCheckpoints, getCheckpointsList, getStartPositions
    , encode, decoder, DecodeResult, DecodeError(..), decodeErrorToString
    , fromMaskBytes, fromMaskBytesList, MaskBytesError(..), maskBytesErrorToString
    , getSurfaces, fromSurfaces
    , SurfacesError(..)
    )

{-|


# Track

@docs Track, Surface, Checkpoint, Size, Position, Velocity, Acceleration
@docs getSize, getSurface, getCheckpoints, getCheckpointsList, getStartPositions


# JSON

@docs encode, decoder, DecodeResult, DecodeError, decodeErrorToString


# Mask

@docs fromMaskBytes, fromMaskBytesList, MaskBytesError, maskBytesErrorToString


# Surfaces

@docs getSurfaces, fromSurfaces, SurfacesError(..)

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Json.Decode
import Json.Encode
import QuadTreeRaster as Raster exposing (Raster)
import VectorRacer.Color as Color exposing (Color)
import VectorRacer.Vector exposing (Vector)
import VectorRacer.Vector.Pixels as Pixels exposing (Pixels)



-- TRACK --


{-| -}
type Track
    = Track Model


type alias Model =
    { surfaces : Raster Surface
    , checkpoints : ( Checkpoint, List Checkpoint )
    , startPositions : List Position
    }


{-| -}
type Surface
    = Road
    | Checkpoint Checkpoint
    | Curb
    | Gravel
    | Wall


{-| -}
type Checkpoint
    = Checkpoint1
    | Checkpoint2
    | Checkpoint3
    | Checkpoint4
    | Checkpoint5


type alias Size =
    Vector Int Pixels


type alias Position =
    Vector Int Pixels


type alias Velocity =
    Vector Int Pixels.PixelsPerStep


type alias Acceleration =
    Vector Int Pixels.PixelsPerStepSquared


{-| -}
getSize : Track -> Size
getSize (Track model) =
    let
        { width, height } =
            Raster.getSize model.surfaces
    in
    Pixels.pixels ( width, height )


{-| -}
getSurface : Position -> Track -> Maybe Surface
getSurface position (Track model) =
    Raster.get (Pixels.inPixels position) model.surfaces


{-| -}
getCheckpoints : Track -> ( Checkpoint, List Checkpoint )
getCheckpoints (Track model) =
    model.checkpoints


{-| -}
getCheckpointsList : Track -> List Checkpoint
getCheckpointsList (Track model) =
    checkpointsTupleToList model.checkpoints


checkpointsTupleToList : ( Checkpoint, List Checkpoint ) -> List Checkpoint
checkpointsTupleToList ( firstCheckpoint, otherCheckpoints ) =
    firstCheckpoint :: otherCheckpoints


{-| -}
getStartPositions : Track -> List Position
getStartPositions (Track model) =
    model.startPositions



-- JSON --


{-| -}
encode : Track -> Json.Encode.Value
encode (Track { surfaces, startPositions }) =
    Json.Encode.object
        [ ( "size", encodeSize (Raster.getSize surfaces) )
        , ( "startPositions", Json.Encode.list encodePosition startPositions )
        , ( "surfaces", Json.Encode.string (serializeSurfaces surfaces) )
        ]


encodeSize : Raster.Size -> Json.Encode.Value
encodeSize size =
    Json.Encode.object
        [ ( "width", Json.Encode.int size.width )
        , ( "height", Json.Encode.int size.height )
        ]


encodePosition : Position -> Json.Encode.Value
encodePosition =
    Pixels.encodePixels Json.Encode.int


serializeSurfaces : Raster Surface -> String
serializeSurfaces surfaces =
    Raster.serialize surfaces
        |> List.foldl (\token result -> encodeSurfaceToken token :: result) []
        |> String.fromList


encodeSurfaceToken : Raster.Token Surface -> Char
encodeSurfaceToken token =
    case token of
        Raster.Branch ->
            'B'

        Raster.Leaf Road ->
            'R'

        Raster.Leaf (Checkpoint checkpoint) ->
            encodeCheckpoint checkpoint

        Raster.Leaf Curb ->
            'C'

        Raster.Leaf Gravel ->
            'G'

        Raster.Leaf Wall ->
            'W'


encodeCheckpoint : Checkpoint -> Char
encodeCheckpoint checkpoint =
    case checkpoint of
        Checkpoint1 ->
            '1'

        Checkpoint2 ->
            '2'

        Checkpoint3 ->
            '3'

        Checkpoint4 ->
            '4'

        Checkpoint5 ->
            '5'


{-| -}
type alias DecodeResult =
    Result DecodeError Track


{-| -}
type DecodeError
    = InvalidSurfaceError Char
    | SurfacesDeserializationError
    | NoCheckpointsError


type alias TrackDecodeData =
    { size : Raster.Size
    , surfaces : String
    , startPositions : List Position
    }


decodeErrorToString : DecodeError -> String
decodeErrorToString error =
    case error of
        InvalidSurfaceError character ->
            "Invalid surface character: " ++ String.fromChar character

        SurfacesDeserializationError ->
            "Surfaces deserialization failed"

        NoCheckpointsError ->
            "No checkpoints found"


{-| -}
decoder : Json.Decode.Decoder DecodeResult
decoder =
    Json.Decode.map decodeTrack dataDecoder


decodeTrack : TrackDecodeData -> Result DecodeError Track
decodeTrack data =
    case deserializeSurfaces data.size data.surfaces of
        Ok surfaces ->
            case findCheckpoints surfaces of
                [] ->
                    Err NoCheckpointsError

                firstCheckpoint :: nextCheckpoints ->
                    Ok
                        (Track
                            { surfaces = surfaces
                            , checkpoints = ( firstCheckpoint, nextCheckpoints )
                            , startPositions = data.startPositions
                            }
                        )

        Err error ->
            Err error


dataDecoder : Json.Decode.Decoder TrackDecodeData
dataDecoder =
    Json.Decode.map3 TrackDecodeData
        (Json.Decode.field "size" sizeDecoder)
        (Json.Decode.field "surfaces" Json.Decode.string)
        (Json.Decode.field "startPositions" (Json.Decode.list positionDecoder)
            |> Json.Decode.maybe
            |> Json.Decode.map (Maybe.withDefault [])
        )


sizeDecoder : Json.Decode.Decoder Raster.Size
sizeDecoder =
    Json.Decode.map2 Raster.Size
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Pixels.pixelsDecoder Json.Decode.int


deserializeSurfaces : Raster.Size -> String -> Result DecodeError (Raster Surface)
deserializeSurfaces size surfaces =
    decodeSurfaceTokens (String.toList surfaces) []
        |> Result.andThen
            (Raster.deserialize size
                >> Maybe.map Ok
                >> Maybe.withDefault (Err SurfacesDeserializationError)
            )


decodeSurfaceTokens : List Char -> List (Raster.Token Surface) -> Result DecodeError (List (Raster.Token Surface))
decodeSurfaceTokens characters tokens =
    case characters of
        [] ->
            Ok tokens

        character :: nextCharacters ->
            case decodeSurfaceToken character of
                Ok token ->
                    decodeSurfaceTokens nextCharacters (token :: tokens)

                Err error ->
                    Err error


decodeSurfaceToken : Char -> Result DecodeError (Raster.Token Surface)
decodeSurfaceToken character =
    case character of
        'B' ->
            Ok Raster.Branch

        'R' ->
            Ok (Raster.Leaf Road)

        'C' ->
            Ok (Raster.Leaf Curb)

        'G' ->
            Ok (Raster.Leaf Gravel)

        'W' ->
            Ok (Raster.Leaf Wall)

        _ ->
            decodeCheckpoint character
                |> Maybe.map (Checkpoint >> Raster.Leaf >> Ok)
                |> Maybe.withDefault (Err (InvalidSurfaceError character))


decodeCheckpoint : Char -> Maybe Checkpoint
decodeCheckpoint character =
    case character of
        '1' ->
            Just Checkpoint1

        '2' ->
            Just Checkpoint2

        '3' ->
            Just Checkpoint3

        '4' ->
            Just Checkpoint4

        '5' ->
            Just Checkpoint5

        _ ->
            Nothing



-- MASK --


type alias MaskBytesResult =
    Result MaskBytesError MaskBytesData


type alias MaskBytesData =
    { surfaces : Raster Surface
    , checkpoints : Checkpoints
    , startPositions : List Position
    }


{-| -}
type MaskBytesError
    = BytesDecodeError
    | InvalidMaskSurfaceError Position Color
    | NoMaskCheckpointsError


maskBytesErrorToString : MaskBytesError -> String
maskBytesErrorToString error =
    case error of
        BytesDecodeError ->
            "Bytes decode error"

        InvalidMaskSurfaceError position color ->
            let
                ( x, y ) =
                    Pixels.inPixels position
            in
            "Invalid surfaces at ("
                ++ String.fromInt x
                ++ ","
                ++ String.fromInt y
                ++ ") :"
                ++ " r:"
                ++ String.fromInt color.r
                ++ " g:"
                ++ String.fromInt color.g
                ++ " b:"
                ++ String.fromInt color.b
                ++ " a:"
                ++ String.fromInt color.a

        NoMaskCheckpointsError ->
            "No checkpoints found"


{-| -}
fromMaskBytes : Size -> Bytes -> Result MaskBytesError Track
fromMaskBytes size maskBytes =
    maskBytes
        |> Bytes.Decode.decode (maskBytesDecoder size)
        |> Maybe.map (Result.andThen maskBytesDataToTrack)
        |> Maybe.withDefault (Err BytesDecodeError)


maskBytesDataToTrack : MaskBytesData -> Result MaskBytesError Track
maskBytesDataToTrack { surfaces, checkpoints, startPositions } =
    case checkpointsToList checkpoints of
        [] ->
            Err NoMaskCheckpointsError

        first :: others ->
            Ok
                (Track
                    { surfaces = surfaces
                    , checkpoints = ( first, others )
                    , startPositions = startPositions
                    }
                )


maskBytesDecoder : Size -> Bytes.Decode.Decoder MaskBytesResult
maskBytesDecoder size =
    let
        ( width, height ) =
            Pixels.inPixels size
    in
    Bytes.Decode.loop
        ( 0
        , { surfaces =
                Raster.init
                    { width = width
                    , height = height
                    }
                    Wall
          , checkpoints =
                { checkpoint1 = False
                , checkpoint2 = False
                , checkpoint3 = False
                , checkpoint4 = False
                , checkpoint5 = False
                }
          , startPositions = []
          }
        )
        maskBytesLoop


maskBytesLoop :
    ( Int, MaskBytesData )
    -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, MaskBytesData ) MaskBytesResult)
maskBytesLoop ( index, { surfaces, checkpoints, startPositions } as data ) =
    let
        size =
            Raster.getSize surfaces

        n =
            size.width * size.height

        position =
            indexToPosition size index
    in
    if index < n then
        Bytes.Decode.map
            (\color ->
                case colorToSurface color of
                    TrackSurface surface ->
                        Bytes.Decode.Loop
                            ( index + 1
                            , { surfaces = updateMaskSurfaceRaster position surface surfaces
                              , checkpoints = updateCheckpoints surface checkpoints
                              , startPositions = startPositions
                              }
                            )

                    StartPointSurface ->
                        Bytes.Decode.Loop
                            ( index + 1
                            , { surfaces = updateMaskSurfaceRaster position Road surfaces
                              , checkpoints = checkpoints
                              , startPositions = position :: startPositions
                              }
                            )

                    InvalidMaskSurface ->
                        Bytes.Decode.Done (Err (InvalidMaskSurfaceError position color))
            )
            Color.bytesDecoder

    else
        Bytes.Decode.succeed (Bytes.Decode.Done (Ok data))


{-| -}
fromMaskBytesList : Size -> List Int -> Result MaskBytesError Track
fromMaskBytesList size maskBytesList =
    let
        ( width, height ) =
            Pixels.inPixels size
    in
    maskBytesList
        |> fromMaskBytesListHelp
            ( 0
            , { surfaces =
                    Raster.init
                        { width = width
                        , height = height
                        }
                        Wall
              , checkpoints =
                    { checkpoint1 = False
                    , checkpoint2 = False
                    , checkpoint3 = False
                    , checkpoint4 = False
                    , checkpoint5 = False
                    }
              , startPositions = []
              }
            )
        |> Result.andThen maskBytesDataToTrack


fromMaskBytesListHelp : ( Int, MaskBytesData ) -> List Int -> Result MaskBytesError MaskBytesData
fromMaskBytesListHelp ( index, data ) bytes =
    let
        size =
            Raster.getSize data.surfaces
    in
    case bytes of
        [] ->
            if index == (size.width * size.height) then
                Ok data

            else
                Err BytesDecodeError

        r :: g :: b :: a :: remainingBytes ->
            let
                position =
                    indexToPosition size index

                color =
                    { r = r
                    , g = g
                    , b = b
                    , a = a
                    }
            in
            case colorToSurface color of
                TrackSurface surface ->
                    fromMaskBytesListHelp
                        ( index + 1
                        , { surfaces = updateMaskSurfaceRaster position surface data.surfaces
                          , checkpoints = updateCheckpoints surface data.checkpoints
                          , startPositions = data.startPositions
                          }
                        )
                        remainingBytes

                StartPointSurface ->
                    fromMaskBytesListHelp
                        ( index + 1
                        , { surfaces = updateMaskSurfaceRaster position Road data.surfaces
                          , checkpoints = data.checkpoints
                          , startPositions = position :: data.startPositions
                          }
                        )
                        remainingBytes

                InvalidMaskSurface ->
                    Err (InvalidMaskSurfaceError position color)

        _ ->
            Err BytesDecodeError


indexToPosition : Raster.Size -> Int -> Position
indexToPosition { width } index =
    Pixels.pixels ( remainderBy width index, index // width )


updateMaskSurfaceRaster : Position -> Surface -> Raster Surface -> Raster Surface
updateMaskSurfaceRaster position surface surfaces =
    Raster.set (Pixels.inPixels position) surface surfaces


type MaskSurface
    = TrackSurface Surface
    | StartPointSurface
    | InvalidMaskSurface


colorToSurface : Color -> MaskSurface
colorToSurface color =
    case ( color.r, color.g, color.b ) of
        ( 0xFF, 0xFF, 0xFF ) ->
            TrackSurface Road

        ( 0x00, 0xFF, 0x00 ) ->
            TrackSurface (Checkpoint Checkpoint1)

        ( 0xFF, 0x00, 0x00 ) ->
            TrackSurface (Checkpoint Checkpoint2)

        ( 0xFF, 0x00, 0xFF ) ->
            TrackSurface (Checkpoint Checkpoint3)

        ( 0xFF, 0x7F, 0x00 ) ->
            TrackSurface (Checkpoint Checkpoint4)

        ( 0x00, 0x00, 0x00 ) ->
            TrackSurface (Checkpoint Checkpoint5)

        ( 0x7F, 0x7F, 0x7F ) ->
            TrackSurface Curb

        ( 0xFF, 0xFF, 0x00 ) ->
            TrackSurface Gravel

        ( 0x00, 0x00, 0xFF ) ->
            TrackSurface Wall

        ( 0x00, 0xFF, 0xFF ) ->
            StartPointSurface

        _ ->
            InvalidMaskSurface



-- SURFACES --


{-| -}
getSurfaces : Track -> Raster Surface
getSurfaces (Track model) =
    model.surfaces


{-| -}
type SurfacesError
    = NoCheckpointSurfacesError
    | InvalidStartPosition Position


{-| -}
fromSurfaces : Raster Surface -> List Position -> Result SurfacesError Track
fromSurfaces surfaces startPositions =
    case checkStartPositions startPositions surfaces of
        Just error ->
            Err error

        Nothing ->
            case findCheckpoints surfaces of
                [] ->
                    Err NoCheckpointSurfacesError

                firstCheckpoint :: nextCheckpoints ->
                    Ok
                        (Track
                            { surfaces = surfaces
                            , checkpoints = ( firstCheckpoint, nextCheckpoints )
                            , startPositions = startPositions
                            }
                        )


checkStartPositions : List Position -> Raster Surface -> Maybe SurfacesError
checkStartPositions startPositions surfaces =
    case startPositions of
        [] ->
            Nothing

        startPosition :: nextStartPositions ->
            case Raster.get (Pixels.inPixels startPosition) surfaces of
                Just surface ->
                    if isValidStartSurface surface then
                        checkStartPositions nextStartPositions surfaces

                    else
                        Just (InvalidStartPosition startPosition)

                Nothing ->
                    Just (InvalidStartPosition startPosition)


isValidStartSurface : Surface -> Bool
isValidStartSurface surface =
    case surface of
        Road ->
            True

        Checkpoint _ ->
            True

        Curb ->
            True

        Gravel ->
            True

        Wall ->
            False


type alias Checkpoints =
    { checkpoint1 : Bool
    , checkpoint2 : Bool
    , checkpoint3 : Bool
    , checkpoint4 : Bool
    , checkpoint5 : Bool
    }


findCheckpoints : Raster Surface -> List Checkpoint
findCheckpoints surfaces =
    Raster.foldlLeaves
        updateCheckpoints
        { checkpoint1 = False
        , checkpoint2 = False
        , checkpoint3 = False
        , checkpoint4 = False
        , checkpoint5 = False
        }
        surfaces
        |> checkpointsToList


checkpointsToList : Checkpoints -> List Checkpoint
checkpointsToList checkpoints =
    List.filterMap identity
        [ maybeCheckpoint Checkpoint1 checkpoints.checkpoint1
        , maybeCheckpoint Checkpoint2 checkpoints.checkpoint2
        , maybeCheckpoint Checkpoint3 checkpoints.checkpoint3
        , maybeCheckpoint Checkpoint4 checkpoints.checkpoint4
        , maybeCheckpoint Checkpoint5 checkpoints.checkpoint5
        ]


maybeCheckpoint : Checkpoint -> Bool -> Maybe Checkpoint
maybeCheckpoint checkpoint enabled =
    if enabled then
        Just checkpoint

    else
        Nothing


updateCheckpoints : Surface -> Checkpoints -> Checkpoints
updateCheckpoints surface checkpoints =
    case surface of
        Checkpoint Checkpoint1 ->
            { checkpoint1 = True
            , checkpoint2 = checkpoints.checkpoint2
            , checkpoint3 = checkpoints.checkpoint3
            , checkpoint4 = checkpoints.checkpoint4
            , checkpoint5 = checkpoints.checkpoint5
            }

        Checkpoint Checkpoint2 ->
            { checkpoint1 = checkpoints.checkpoint1
            , checkpoint2 = True
            , checkpoint3 = checkpoints.checkpoint3
            , checkpoint4 = checkpoints.checkpoint4
            , checkpoint5 = checkpoints.checkpoint5
            }

        Checkpoint Checkpoint3 ->
            { checkpoint1 = checkpoints.checkpoint1
            , checkpoint2 = checkpoints.checkpoint2
            , checkpoint3 = True
            , checkpoint4 = checkpoints.checkpoint4
            , checkpoint5 = checkpoints.checkpoint5
            }

        Checkpoint Checkpoint4 ->
            { checkpoint1 = checkpoints.checkpoint1
            , checkpoint2 = checkpoints.checkpoint2
            , checkpoint3 = checkpoints.checkpoint3
            , checkpoint4 = True
            , checkpoint5 = checkpoints.checkpoint5
            }

        Checkpoint Checkpoint5 ->
            { checkpoint1 = checkpoints.checkpoint1
            , checkpoint2 = checkpoints.checkpoint2
            , checkpoint3 = checkpoints.checkpoint3
            , checkpoint4 = checkpoints.checkpoint4
            , checkpoint5 = True
            }

        _ ->
            checkpoints
