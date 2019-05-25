module VectorRacer.Ui.TrackPanel exposing
    ( Size
    , TrackPanel
    , init
    , setGrid
    , setPanelSize
    , setTrack
    , view
    )

import Element exposing (Element)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes
import VectorRacer.Grid exposing (Grid)
import VectorRacer.Pixels as Pixels exposing (Pixels)
import VectorRacer.Track as Track
import VectorRacer.Vector as Vector exposing (Vector)



-- MODEL --


type TrackPanel
    = TrackPanel Model


type alias Model =
    { panelSize : Size
    , trackSize : Size
    , trackImage : String
    , grid : Maybe Grid
    , offset : Offset
    , scale : Scale
    , state : State
    }


type alias Size =
    Vector Int Pixels


type alias Offset =
    Vector Float Pixels


type alias Scale =
    Vector Float Quantity.Unitless


type State
    = Inactive
    | MouseActive MouseState
    | TouchActive (List TouchData)


type alias MouseState =
    { down : ( Float, Float )
    , current : ( Float, Float )
    }


type alias TouchData =
    { id : Int
    , start : ( Float, Float )
    , current : ( Float, Float )
    }


init :
    { panelSize : Size
    , trackSize : Size
    , trackImage : String
    }
    -> TrackPanel
init { panelSize, trackSize, trackImage } =
    TrackPanel
        { panelSize = panelSize
        , trackSize = trackSize
        , trackImage = trackImage
        , grid = Nothing
        , offset = Pixels.pixels 0 0
        , scale = Vector.fromFloats 1 1
        , state = Inactive
        }


setGrid : Maybe Grid -> TrackPanel -> TrackPanel
setGrid grid (TrackPanel model) =
    TrackPanel { model | grid = grid }


setPanelSize : Size -> TrackPanel -> TrackPanel
setPanelSize panelSize (TrackPanel model) =
    TrackPanel { model | panelSize = panelSize }


setTrack : Track.Size -> String -> TrackPanel -> TrackPanel
setTrack trackSize trackImage (TrackPanel model) =
    TrackPanel
        { model
            | trackSize = trackSize
            , trackImage = trackImage
        }



-- TODO: centerOn...
-- UPDATE --
-- SUBSCRIPTIONS --
-- VIEW --


view : TrackPanel -> Element msg
view (TrackPanel model) =
    let
        ( width, height ) =
            Pixels.inPixels model.panelSize

        ( trackMarginX, trackMarginY ) =
            model.trackSize
                |> Vector.divideByInt (Vector.fromInts 10 10)
                |> Pixels.inPixels

        ( trackWidth, trackHeight ) =
            Pixels.inPixels model.trackSize
    in
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width (String.fromInt width ++ "px")
            , Svg.Attributes.height (String.fromInt height ++ "px")
            ]
            [ Svg.g
                [ Svg.Attributes.transform
                    (String.join ""
                        [ "translate("
                        , String.fromInt trackMarginX
                        , ","
                        , String.fromInt trackMarginY
                        , ")"
                        ]
                    )
                ]
                [ Svg.image
                    [ Svg.Attributes.xlinkHref "example.png"
                    , Svg.Attributes.width (String.fromInt trackWidth)
                    , Svg.Attributes.height (String.fromInt trackHeight)
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.x "-1"
                    , Svg.Attributes.x "-1"
                    , Svg.Attributes.width (String.fromInt (trackWidth + 2))
                    , Svg.Attributes.height (String.fromInt (trackHeight + 2))
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "#3C78F0"
                    , Svg.Attributes.strokeWidth "2"
                    ]
                    []
                ]
            ]
