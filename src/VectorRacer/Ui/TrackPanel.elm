module VectorRacer.Ui.TrackPanel exposing
    ( TrackPanel
    , init
    , setGrid
    , setPanelSize
    , setTrack
    , view
    )

import Svg exposing (Svg)
import Svg.Attributes
import VectorRacer.Grid exposing (Grid)
import VectorRacer.Vector as Vector exposing (OffsetVector, ScaleVector, SizeVector, Vector)



-- MODEL --


type TrackPanel
    = TrackPanel Model


type alias Model =
    { panelSize : SizeVector
    , trackSize : SizeVector
    , trackImage : String
    , grid : Maybe Grid
    , offset : OffsetVector
    , scale : ScaleVector
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
    { panelSize : SizeVector
    , trackSize : SizeVector
    , trackImage : String
    }
    -> TrackPanel
init { panelSize, trackSize, trackImage } =
    TrackPanel
        { panelSize = panelSize
        , trackSize = trackSize
        , trackImage = trackImage
        , grid = Nothing
        , offset = Vector.init 0 0
        , scale = Vector.init 1 1
        , state = Inactive
        }


setGrid : Maybe Grid -> TrackPanel -> TrackPanel
setGrid grid (TrackPanel model) =
    TrackPanel { model | grid = grid }


setPanelSize : SizeVector -> TrackPanel -> TrackPanel
setPanelSize panelSize (TrackPanel model) =
    TrackPanel { model | panelSize = panelSize }


setTrack : SizeVector -> String -> TrackPanel -> TrackPanel
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


view : TrackPanel -> Svg msg
view (TrackPanel model) =
    let
        trackMargin =
            model.trackSize |> Vector.divideBy (Vector.init 10 10)
    in
    Svg.svg
        [ Svg.Attributes.width (Vector.xToPx model.panelSize)
        , Svg.Attributes.height (Vector.yToPx model.panelSize)
        ]
        [ Svg.g
            [ Svg.Attributes.transform ("translate" ++ Vector.toTupleString trackMargin)
            ]
            [ Svg.image
                [ Svg.Attributes.xlinkHref "example.png"
                , Svg.Attributes.x "1px"
                , Svg.Attributes.y "1px"
                , Svg.Attributes.width (Vector.xToPx model.trackSize)
                , Svg.Attributes.height (Vector.yToPx model.trackSize)
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width (Vector.x model.trackSize + 2 |> String.fromInt)
                , Svg.Attributes.height (Vector.y model.trackSize + 2 |> String.fromInt)
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "#3C78F0"
                , Svg.Attributes.strokeWidth "2"
                ]
                []
            ]
        ]
