module VectorRacer.Ui.PanZoom exposing
    ( Msg
    , PanZoom
    , Point
    , Scale
    , events
    , getTransform
    , getTransformString
    , init
    , subscriptions
    , toLocal
    , update
    , withOffset
    , withScale
    , withScaleBounds
    )

import Browser.Events
import Html
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events.Extra.Wheel as Wheel
import Json.Decode
import Quantity exposing (Quantity)
import Quantity.Interval as Interval exposing (Interval)
import Quantity.Vector2 as Vector2 exposing (Vector2)
import VectorRacer.Pixels as Pixels exposing (Pixels)



-- MODEL --


type PanZoom
    = PanZoom Model


type alias Model =
    { config : Config
    , state : State
    }


type alias Config =
    { scaleInterval : Interval Float Quantity.Unitless
    }


type State
    = Inactive InactiveState
    | MouseActive MouseState
    | TouchActive TouchState


type alias InactiveState =
    { offset : Point
    , scale : Scale
    , lastEventPoint : Point
    }


type alias MouseState =
    { baseOffset : Point
    , baseScale : Scale
    , down : Point
    , current : Point
    }


type alias TouchState =
    { baseOffset : Point
    , baseScale : Scale
    , touches : List TouchData
    }


type alias TouchData =
    { id : Int
    , start : Point
    , current : Point
    }


type alias Point =
    Vector2 Float Pixels


type alias Scale =
    Quantity Float Quantity.Unitless


init : PanZoom
init =
    PanZoom
        { config = { scaleInterval = Interval.from (Quantity.float 0.1) (Quantity.float 10) }
        , state =
            Inactive
                { offset = Pixels.pixels ( 0, 0 )
                , scale = Quantity.float 1
                , lastEventPoint = Pixels.pixels ( 0, 0 )
                }
        }


withScaleBounds : Interval Float Quantity.Unitless -> PanZoom -> PanZoom
withScaleBounds scaleInterval (PanZoom { config, state }) =
    let
        newConfig =
            { config | scaleInterval = scaleInterval }
    in
    PanZoom
        { config = newConfig
        , state = checkBounds newConfig state
        }


withOffset : Point -> PanZoom -> PanZoom
withOffset offset (PanZoom { config, state }) =
    let
        { scale } =
            getStateTransform state

        newState =
            resetStateWith { offset = offset, scale = scale } state
    in
    PanZoom
        { config = config
        , state = checkBounds config newState
        }


withScale : Scale -> Point -> PanZoom -> PanZoom
withScale scale scaleCenter (PanZoom { config, state }) =
    let
        { offset } =
            getStateTransform state

        newState =
            updateScale scale (scaleCenter |> Vector2.plus offset) state
    in
    PanZoom
        { config = config
        , state = checkBounds config newState
        }


getTransform : PanZoom -> { offset : Point, scale : Scale }
getTransform (PanZoom { state }) =
    getStateTransform state


getStateTransform : State -> { offset : Point, scale : Scale }
getStateTransform state =
    case state of
        Inactive { offset, scale } ->
            { offset = offset
            , scale = scale
            }

        MouseActive { baseOffset, baseScale, down, current } ->
            { offset = baseOffset |> Vector2.plus current |> Vector2.minus down
            , scale = baseScale
            }

        TouchActive { baseOffset, baseScale, touches } ->
            case touches of
                touchA :: touchB :: _ ->
                    let
                        distance0 =
                            Vector2.distance touchA.start touchB.start

                        distance1 =
                            Vector2.distance touchA.current touchB.current

                        zoom =
                            Quantity.ratio distance1 distance0

                        zoomVector =
                            Vector2.fromFloat zoom

                        mean0 =
                            Vector2.mean touchA.start touchB.start

                        mean1 =
                            Vector2.mean touchA.current touchB.current
                    in
                    { offset = mean1 |> Vector2.minus (mean0 |> Vector2.minus baseOffset |> Vector2.multiplyBy zoomVector)
                    , scale = baseScale |> Quantity.multiplyBy zoom
                    }

                touch :: _ ->
                    { offset = baseOffset |> Vector2.plus (touch.current |> Vector2.minus touch.start)
                    , scale = baseScale
                    }

                _ ->
                    { offset = baseOffset, scale = baseScale }


getTransformString : PanZoom -> String
getTransformString model =
    let
        transform =
            getTransform model

        ( dx, dy ) =
            Pixels.inPixels transform.offset

        scale =
            Quantity.toFloat transform.scale
    in
    "translate("
        ++ String.fromFloat dx
        ++ ","
        ++ String.fromFloat dy
        ++ ") scale("
        ++ String.fromFloat scale
        ++ ")"


toLocal : PanZoom -> Point -> Point
toLocal model point =
    let
        { offset, scale } =
            getTransform model
    in
    point
        |> Vector2.minus offset
        |> Vector2.divideBy (Vector2.fromQuantity scale)



-- UPDATE --


type Msg
    = MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | MouseUp Mouse.Event
    | TouchStart Touch.Event
    | TouchMove Touch.Event
    | TouchEnd Touch.Event
    | TouchCancel Touch.Event
    | Wheel Wheel.Event


update : Msg -> PanZoom -> PanZoom
update msg (PanZoom { config, state }) =
    PanZoom
        { config = config
        , state = updateState msg state |> checkBounds config
        }


updateState : Msg -> State -> State
updateState msg state =
    case msg of
        MouseDown event ->
            let
                { offset, scale } =
                    getStateTransform state
            in
            MouseActive
                { baseOffset = offset
                , baseScale = scale
                , down = Pixels.pixels event.pagePos
                , current = Pixels.pixels event.pagePos
                }

        MouseMove event ->
            case state of
                Inactive _ ->
                    state

                MouseActive mouseState ->
                    MouseActive { mouseState | current = Pixels.pixels event.pagePos }

                TouchActive _ ->
                    state

        MouseUp event ->
            case state of
                Inactive _ ->
                    state

                MouseActive _ ->
                    Inactive (getInactiveState (Pixels.pixels event.pagePos) state)

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

        Wheel event ->
            case state of
                Inactive { offset, scale } ->
                    let
                        zoom =
                            updateZoom event offset scale
                    in
                    Inactive
                        { offset = zoom.offset
                        , scale = zoom.scale
                        , lastEventPoint = zoom.eventPoint
                        }

                MouseActive mouse ->
                    -- TODO: TEST
                    let
                        zoom =
                            updateZoom event mouse.baseOffset mouse.baseScale
                    in
                    MouseActive
                        { mouse
                            | baseOffset = zoom.offset
                            , baseScale = zoom.scale
                        }

                TouchActive _ ->
                    state


updateScale : Scale -> Point -> State -> State
updateScale newScale center state =
    let
        { offset, scale } =
            getStateTransform state

        newZoom =
            Quantity.ratio newScale scale

        newOffset =
            updateOffsetWithZoom newZoom center offset
    in
    resetStateWith { offset = newOffset, scale = newScale } state


updateOffsetWithZoom : Float -> Point -> Point -> Point
updateOffsetWithZoom zoom center offset =
    let
        offsetDiff =
            offset |> Vector2.minus center

        offsetDiffZoomed =
            offsetDiff |> Vector2.multiplyBy (Vector2.fromFloat zoom)
    in
    offset |> Vector2.plus offsetDiffZoomed |> Vector2.minus offsetDiff


resetStateWith : { offset : Point, scale : Scale } -> State -> State
resetStateWith { offset, scale } state =
    case state of
        Inactive _ ->
            Inactive
                { offset = offset
                , scale = scale
                , lastEventPoint = offset
                }

        MouseActive { current } ->
            MouseActive
                { baseOffset = offset
                , baseScale = scale
                , down = current
                , current = current
                }

        TouchActive { touches } ->
            TouchActive
                { baseOffset = offset
                , baseScale = scale
                , touches = touches |> List.map (\touch -> { touch | start = touch.current })
                }


getInactiveState : Point -> State -> InactiveState
getInactiveState eventPoint state =
    let
        { offset, scale } =
            getStateTransform state
    in
    { offset = offset
    , scale = scale
    , lastEventPoint = eventPoint
    }


endTouches : Touch.Event -> State -> State
endTouches event state =
    case state of
        Inactive _ ->
            state

        MouseActive _ ->
            state

        TouchActive { touches } ->
            if List.isEmpty event.touches then
                let
                    touchOffset =
                        getTouchOffset touches |> Maybe.withDefault (getStateTransform state).offset
                in
                Inactive (getInactiveState touchOffset state)

            else
                resetTouches event state


getTouchOffset : List TouchData -> Maybe Point
getTouchOffset touches =
    case touches of
        [] ->
            Nothing

        touch :: [] ->
            Just touch.current

        touch1 :: touch2 :: _ ->
            Just (Vector2.mean touch1.current touch2.current)


resetTouches : Touch.Event -> State -> State
resetTouches event state =
    let
        { offset, scale } =
            getStateTransform state
    in
    TouchActive
        { baseOffset = offset
        , baseScale = scale
        , touches =
            List.map
                (\touch ->
                    { id = touch.identifier
                    , start = Pixels.pixels touch.pagePos
                    , current = Pixels.pixels touch.pagePos
                    }
                )
                event.touches
        }


updateTouches : Touch.Touch -> List TouchData -> List TouchData
updateTouches touch touches =
    List.map
        (\touchData ->
            if touchData.id == touch.identifier then
                { touchData | current = Pixels.pixels touch.pagePos }

            else
                touchData
        )
        touches


updateZoom : Wheel.Event -> Point -> Scale -> { offset : Point, scale : Scale, eventPoint : Point }
updateZoom event offset scale =
    let
        zoom =
            wheelZoom event

        eventPoint =
            Pixels.pixels event.mouseEvent.pagePos
    in
    { offset = updateOffsetWithZoom zoom eventPoint offset
    , scale = Quantity.multiplyBy zoom scale
    , eventPoint = eventPoint
    }


wheelZoom : Wheel.Event -> Float
wheelZoom event =
    case event.deltaMode of
        Wheel.DeltaPixel ->
            1 + event.deltaY * 0.0015

        Wheel.DeltaLine ->
            1 + event.deltaY * 0.075

        Wheel.DeltaPage ->
            1 + event.deltaY * 0.15


checkBounds : Config -> State -> State
checkBounds config state =
    let
        { offset, scale } =
            getStateTransform state

        { scaleInterval } =
            config
    in
    if Interval.contains scale scaleInterval then
        state

    else
        let
            newScale =
                if scale |> Quantity.lessThan (Interval.minValue scaleInterval) then
                    Interval.minValue scaleInterval

                else
                    Interval.maxValue scaleInterval

            zoomCenter =
                case state of
                    Inactive { lastEventPoint } ->
                        lastEventPoint

                    MouseActive { current } ->
                        current

                    TouchActive { touches } ->
                        case getTouchOffset touches of
                            Just touchOffset ->
                                touchOffset

                            Nothing ->
                                offset
        in
        updateScale newScale zoomCenter state



-- SUBSCRIPTIONS --


subscriptions : PanZoom -> Sub Msg
subscriptions (PanZoom { state }) =
    case state of
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


events : List (Html.Attribute Msg)
events =
    [ Mouse.onWithOptions "mousedown" mouseDownOptions MouseDown
    , Touch.onStart TouchStart
    , Touch.onMove TouchMove
    , Touch.onEnd TouchEnd
    , Touch.onCancel TouchCancel
    , Wheel.onWheel Wheel
    ]


mouseDownOptions : Mouse.EventOptions
mouseDownOptions =
    { stopPropagation = True
    , preventDefault = True
    }
