module VectorRacer.Ui.TrackPanel exposing
    ( TrackPanel, Size, init, setGrid, setPanelSize, setTrack
    , Msg, update, subscriptions
    , view
    )

{-|

@docs TrackPanel, Size, init, setGrid, setPanelSize, setTrack
@docs Msg, update, subscriptions
@docs view

-}

import Element exposing (Element)
import Quantity
import Quantity.Interval as Interval
import Svg exposing (Svg)
import Svg.Attributes
import VectorRacer.Grid as Grid exposing (Grid)
import VectorRacer.Track as Track
import VectorRacer.Ui.PanZoom as PanZoom exposing (PanZoom)
import VectorRacer.Vector as Vector exposing (Vector)
import VectorRacer.Vector.Pixels as Pixels exposing (Pixels)



-- MODEL --


type TrackPanel
    = TrackPanel Model


type alias Model =
    { panelSize : Size
    , trackSize : Size
    , trackImage : String
    , grid : Maybe Grid
    , panZoom : PanZoom
    }


type alias Size =
    Vector Int Pixels


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
        , panZoom =
            PanZoom.init
                |> PanZoom.withScaleBounds (Interval.from (Quantity.float 0.5) (Quantity.float 2.5))
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



-- UPDATE --


type Msg
    = GotPanZoomMsg PanZoom.Msg


update : Msg -> TrackPanel -> TrackPanel
update msg (TrackPanel model) =
    case msg of
        GotPanZoomMsg panZoomMsg ->
            TrackPanel { model | panZoom = PanZoom.update panZoomMsg model.panZoom }



-- SUBSCRIPTIONS --


subscriptions : TrackPanel -> Sub Msg
subscriptions (TrackPanel model) =
    PanZoom.subscriptions model.panZoom |> Sub.map GotPanZoomMsg



-- VIEW --


view : TrackPanel -> Element Msg
view (TrackPanel model) =
    let
        ( panelWidth, panelHeight ) =
            Pixels.inPixels model.panelSize

        ( trackWidth, trackHeight ) =
            Pixels.inPixels model.trackSize

        transform =
            PanZoom.getTransformString model.panZoom
    in
    Element.html <|
        Svg.svg
            ([ Svg.Attributes.width (String.fromInt panelWidth ++ "px")
             , Svg.Attributes.height (String.fromInt panelHeight ++ "px")
             ]
                |> PanZoom.withEvents GotPanZoomMsg
            )
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
                    , Svg.Attributes.y "-1"
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
                        |> Vector.minus (Pixels.pixels ( 0.5, 0.5 ))
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
