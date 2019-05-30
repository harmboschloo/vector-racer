module VectorRacer.Ui.PanZoom exposing
    ( Msg
    , PanZoom
    , Point
    , Scale
    , getTransform
    , getTransformString
    , init
    , subscriptions
    , update
    , withEvents
    , withScaleBounds
    )

import Browser.Events
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events.Extra.Wheel as Wheel
import Json.Decode
import Quantity exposing (Quantity)
import Quantity.Interval as Interval exposing (Interval)
import VectorRacer.Vector as Vector exposing (Vector)
import VectorRacer.Vector.Pixels as Pixels exposing (Pixels)



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
    Vector Float Pixels


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
                            Vector.fromFloat zoom

                        mean0 =
                            Vector.mean touchA.start touchB.start

                        mean1 =
                            Vector.mean touchA.current touchB.current
                    in
                    { offset = mean1 |> Vector.minus (mean0 |> Vector.minus baseOffset |> Vector.multiplyBy zoomVector)
                    , scale = baseScale |> Quantity.multiplyBy zoom
                    }

                touch :: _ ->
                    { offset = baseOffset |> Vector.plus (touch.current |> Vector.minus touch.start)
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

        MouseUp event ->
            case state of
                Inactive _ ->
                    state

                MouseActive _ ->
                    Inactive (getInactiveState (Pixels.pixels event.clientPos) state)

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
            Just (Vector.mean touch1.current touch2.current)


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


updateZoom : Wheel.Event -> Point -> Scale -> { offset : Point, scale : Scale, eventPoint : Point }
updateZoom event offset scale =
    let
        zoom =
            wheelZoom event

        eventPoint =
            Pixels.pixels event.mouseEvent.clientPos
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


updateOffsetWithZoom : Float -> Point -> Point -> Point
updateOffsetWithZoom zoom eventPoint offset =
    let
        offsetDiff =
            offset |> Vector.minus eventPoint

        offsetDiffZoomed =
            offsetDiff |> Vector.multiplyBy (Vector.fromFloat zoom)
    in
    offset |> Vector.plus offsetDiffZoomed |> Vector.minus offsetDiff


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

            newZoom =
                Quantity.ratio newScale scale
        in
        case state of
            Inactive { lastEventPoint } ->
                let
                    newOffset =
                        updateOffsetWithZoom newZoom lastEventPoint offset
                in
                Inactive
                    { offset = newOffset
                    , scale = newScale
                    , lastEventPoint = lastEventPoint
                    }

            MouseActive { current } ->
                let
                    newOffset =
                        updateOffsetWithZoom newZoom current offset
                in
                MouseActive
                    { baseOffset = newOffset
                    , baseScale = newScale
                    , down = current
                    , current = current
                    }

            TouchActive { touches } ->
                let
                    newOffset =
                        case getTouchOffset touches of
                            Just touchOffset ->
                                updateOffsetWithZoom newZoom touchOffset offset

                            Nothing ->
                                offset
                in
                TouchActive
                    { baseOffset = newOffset
                    , baseScale = newScale
                    , touches = touches |> List.map (\touch -> { touch | start = touch.current })
                    }



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


withEvents : (Msg -> msg) -> List (Html.Attribute msg) -> List (Html.Attribute msg)
withEvents toMsg attributes =
    (Mouse.onWithOptions "mousedown" mouseDownOptions MouseDown |> Html.Attributes.map toMsg)
        :: (Touch.onStart TouchStart |> Html.Attributes.map toMsg)
        :: (Touch.onMove TouchMove |> Html.Attributes.map toMsg)
        :: (Touch.onEnd TouchEnd |> Html.Attributes.map toMsg)
        :: (Touch.onCancel TouchCancel |> Html.Attributes.map toMsg)
        :: (Wheel.onWheel Wheel |> Html.Attributes.map toMsg)
        :: attributes


mouseDownOptions : Mouse.EventOptions
mouseDownOptions =
    { stopPropagation = True
    , preventDefault = True
    }
