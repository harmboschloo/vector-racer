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

import Browser.Events
import Element exposing (Element)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode
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
    , state : State
    }


type alias Size =
    Vector Int Pixels


type alias Offset =
    Vector Float Pixels


type alias Point =
    Vector Float Pixels


type alias Scale =
    Vector Float Quantity.Unitless


type State
    = Inactive InactiveState
    | MouseActive MouseState
    | TouchActive TouchState


type alias InactiveState =
    { offset : Offset
    , scale : Scale
    }


type alias MouseState =
    { baseOffset : Offset
    , baseScale : Scale
    , down : Point
    , current : Point
    }


type alias TouchState =
    { baseOffset : Offset
    , baseScale : Scale
    , touches : List TouchData
    }


type alias TouchData =
    { id : Int
    , start : Point
    , current : Point
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
        , state =
            Inactive
                { offset = Pixels.pixels ( 0, 0 )
                , scale = Vector.fromFloats ( 2.5, 2.5 )
                }
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


getTransform : State -> { offset : Offset, scale : Scale }
getTransform state =
    case state of
        Inactive transform ->
            transform

        MouseActive { baseOffset, baseScale, down, current } ->
            { offset = baseOffset |> Vector.plus current |> Vector.minus down
            , scale = baseScale
            }

        TouchActive { baseOffset, baseScale, touches } ->
            case touches of
                touchA :: touchB :: _ ->
                    let
                        distance0 =
                            Vector.distance touchA.start touchB.start

                        distance1 =
                            Vector.distance touchA.current touchB.current

                        zoom =
                            Quantity.ratio distance1 distance0

                        zoomVector =
                            Vector.fromFloats ( zoom, zoom )

                        mean0 =
                            Vector.mean touchA.start touchB.start

                        mean1 =
                            Vector.mean touchA.current touchB.current
                    in
                    { offset = mean1 |> Vector.minus (mean0 |> Vector.minus baseOffset |> Vector.multiplyBy zoomVector)
                    , scale = baseScale |> Vector.multiplyBy zoomVector
                    }

                touch :: _ ->
                    { offset = baseOffset |> Vector.plus (touch.current |> Vector.minus touch.start)
                    , scale = baseScale
                    }

                _ ->
                    { offset = baseOffset, scale = baseScale }


getTransformString : State -> String
getTransformString state =
    let
        { offset, scale } =
            getTransform state

        ( dx, dy ) =
            Pixels.inPixels offset

        ( scaleX, scaleY ) =
            Vector.toFloats scale
    in
    "translate("
        ++ String.fromFloat dx
        ++ ","
        ++ String.fromFloat dy
        ++ ") scale("
        ++ String.fromFloat scaleX
        ++ ",0"
        ++ String.fromFloat scaleY
        ++ ")"



-- UPDATE --


type Msg
    = MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | MouseUp Mouse.Event
    | TouchStart Touch.Event
    | TouchMove Touch.Event
    | TouchEnd Touch.Event
    | TouchCancel Touch.Event


update : Msg -> TrackPanel -> TrackPanel
update msg (TrackPanel model) =
    TrackPanel { model | state = updateState msg model.state }


updateState : Msg -> State -> State
updateState msg state =
    case msg of
        MouseDown event ->
            let
                { offset, scale } =
                    getTransform state
            in
            MouseActive
                { baseOffset = offset
                , baseScale = scale
                , down = Pixels.pixels event.clientPos
                , current = Pixels.pixels event.clientPos
                }

        MouseMove event ->
            case state of
                Inactive _ ->
                    state

                MouseActive mouseState ->
                    MouseActive { mouseState | current = Pixels.pixels event.clientPos }

                TouchActive _ ->
                    state

        MouseUp _ ->
            case state of
                Inactive _ ->
                    state

                MouseActive _ ->
                    Inactive (getTransform state)

                TouchActive _ ->
                    state

        TouchStart event ->
            resetTouches event state

        TouchMove event ->
            case state of
                Inactive _ ->
                    state

                MouseActive _ ->
                    state

                TouchActive touchState ->
                    TouchActive
                        { touchState | touches = List.foldl updateTouches touchState.touches event.changedTouches }

        TouchEnd event ->
            endTouches event state

        TouchCancel event ->
            endTouches event state


endTouches : Touch.Event -> State -> State
endTouches event state =
    case state of
        Inactive _ ->
            state

        MouseActive _ ->
            state

        TouchActive _ ->
            if List.isEmpty event.touches then
                Inactive (getTransform state)

            else
                resetTouches event state


resetTouches : Touch.Event -> State -> State
resetTouches event state =
    let
        { offset, scale } =
            getTransform state
    in
    TouchActive
        { baseOffset = offset
        , baseScale = scale
        , touches =
            List.map
                (\touch ->
                    { id = touch.identifier
                    , start = Pixels.pixels touch.clientPos
                    , current = Pixels.pixels touch.clientPos
                    }
                )
                event.touches
        }


updateTouches : Touch.Touch -> List TouchData -> List TouchData
updateTouches touch touches =
    List.map
        (\touchData ->
            if touchData.id == touch.identifier then
                { touchData | current = Pixels.pixels touch.clientPos }

            else
                touchData
        )
        touches



-- SUBSCRIPTIONS --


subscriptions : TrackPanel -> Sub Msg
subscriptions (TrackPanel model) =
    case model.state of
        Inactive _ ->
            Sub.none

        MouseActive _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Json.Decode.map MouseMove Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Json.Decode.map MouseUp Mouse.eventDecoder)
                ]

        TouchActive _ ->
            Sub.none



-- VIEW --


view : TrackPanel -> Element Msg
view (TrackPanel model) =
    let
        ( panelWidth, panelHeight ) =
            Pixels.inPixels model.panelSize

        --        ( trackMarginX, trackMarginY ) =
        --            model.trackSize
        --                |> Vector.divideByInt (Vector.fromInts ( 10, 10 ))
        --                |> Pixels.inPixels
        ( trackWidth, trackHeight ) =
            Pixels.inPixels model.trackSize

        --        transform =
        --            String.join ""
        --                [ "translate("
        --                , String.fromInt trackMarginX
        --                , ","
        --                , String.fromInt trackMarginY
        --                , ")"
        --
        --                --                , "), scale(2,2)"
        --                ]
        transform =
            getTransformString model.state
    in
    Element.html <|
        Svg.svg
            ([ Svg.Attributes.width (String.fromInt panelWidth ++ "px")
             , Svg.Attributes.height (String.fromInt panelHeight ++ "px")
             ]
                |> withEvents
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


withEvents : List (Svg.Attribute Msg) -> List (Svg.Attribute Msg)
withEvents attributes =
    Mouse.onWithOptions
        "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        MouseDown
        :: Touch.onStart TouchStart
        :: Touch.onMove TouchMove
        :: Touch.onEnd TouchEnd
        :: Touch.onCancel TouchCancel
        :: attributes


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
