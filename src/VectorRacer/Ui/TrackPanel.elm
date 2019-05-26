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
import VectorRacer.Grid as Grid exposing (Grid)
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
        ( panelWidth, panelHeight ) =
            Pixels.inPixels model.panelSize

        ( trackMarginX, trackMarginY ) =
            model.trackSize
                |> Vector.divideByInt (Vector.fromInts 10 10)
                |> Pixels.inPixels

        ( trackWidth, trackHeight ) =
            Pixels.inPixels model.trackSize

        transform =
            String.join ""
                [ "translate("
                , String.fromInt trackMarginX
                , ","
                , String.fromInt trackMarginY
                , ")"

                --                , "), scale(2,2)"
                ]
    in
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width (String.fromInt panelWidth ++ "px")
            , Svg.Attributes.height (String.fromInt panelHeight ++ "px")
            ]
            [ Svg.defs [] ([] |> withGridDef model.grid transform)
            , Svg.g
                [ Svg.Attributes.transform transform
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
            , viewGrid model.grid model.panelSize
            ]


withGridDef : Maybe Grid -> String -> List (Svg msg) -> List (Svg msg)
withGridDef maybeGrid transform elements =
    case maybeGrid of
        Just grid ->
            let
                ( anchorX, anchorY ) =
                    Pixels.inPixels (Grid.getAnchorPoint grid)

                ( spacingX, spacingY ) =
                    Pixels.inPixels (Grid.getSpacing grid)

                ( farPointX, farPointY ) =
                    Grid.getSpacing grid
                        |> Vector.toFloatVector
                        |> Vector.minus (Pixels.pixels 0.5 0.5)
                        |> Pixels.inPixels
                        |> Tuple.mapBoth String.fromFloat String.fromFloat
            in
            Svg.pattern
                [ Svg.Attributes.id "grid"
                , Svg.Attributes.x (String.fromInt anchorX)
                , Svg.Attributes.y (String.fromInt anchorY)
                , Svg.Attributes.width (String.fromInt spacingX)
                , Svg.Attributes.height (String.fromInt spacingY)
                , Svg.Attributes.patternUnits "userSpaceOnUse"
                , Svg.Attributes.patternTransform transform
                ]
                [ Svg.polyline
                    [ Svg.Attributes.points ("0.5 " ++ farPointY ++ ", 0.5 0.5 " ++ farPointX ++ " 0.5")
                    , Svg.Attributes.stroke "#3C78F0"
                    , Svg.Attributes.strokeOpacity "0.2"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.strokeLinecap "square"
                    ]
                    []
                ]
                :: elements

        Nothing ->
            elements


viewGrid : Maybe Grid -> Size -> Svg msg
viewGrid maybeGrid size =
    case maybeGrid of
        Just _ ->
            let
                ( width, height ) =
                    Pixels.inPixels size
            in
            Svg.rect
                [ Svg.Attributes.fill "url(#grid)"
                , Svg.Attributes.width (String.fromInt width)
                , Svg.Attributes.height (String.fromInt height)
                ]
                []

        Nothing ->
            Svg.g [] []
