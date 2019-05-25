module VectorRacer.Ui.TrackPanel exposing
    ( TrackPanel
    , init
    , setGrid
    , setPanelSize
    , setTrack
    , view
    )

import Element exposing (Element)
import Svg exposing (Svg)
import Svg.Attributes
import VectorRacer.Grid exposing (Grid)
import VectorRacer.Pixels as Pixels exposing (Pixels)
import VectorRacer.Track as Track
import VectorRacer.Ui as Ui
import VectorRacer.Vector as Vector exposing (Vector)



-- MODEL --


type TrackPanel
    = TrackPanel Model


type alias Model =
    { panelSize : Ui.Size
    , trackSize : Ui.Size
    , trackImage : String
    , grid : Maybe Grid
    , offset : Ui.Offset
    , scale : Ui.Scale
    , state : State
    }


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
    { panelSize : Ui.Size
    , trackSize : Ui.Size
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


setPanelSize : Ui.Size -> TrackPanel -> TrackPanel
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
        trackMargin =
            model.trackSize
                |> Vector.divideByInt (Vector.fromInts 10 10)
                |> Vector.toFloatVector

        trackBorderSize =
            model.trackSize |> Vector.plus (Pixels.pixels 2 2)
    in
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width (Ui.xToPx model.panelSize)
            , Svg.Attributes.height (Ui.yToPx model.panelSize)
            ]
            [ Svg.g
                [ Svg.Attributes.transform ("translate" ++ Ui.toTupleString trackMargin)
                ]
                [ Svg.image
                    [ Svg.Attributes.xlinkHref "example.png"
                    , Svg.Attributes.x "1px"
                    , Svg.Attributes.y "1px"
                    , Svg.Attributes.width (Ui.xToPx model.trackSize)
                    , Svg.Attributes.height (Ui.yToPx model.trackSize)
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.width (Ui.xToString trackBorderSize)
                    , Svg.Attributes.height (Ui.yToString trackBorderSize)
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "#3C78F0"
                    , Svg.Attributes.strokeWidth "2"
                    ]
                    []
                ]
            ]
