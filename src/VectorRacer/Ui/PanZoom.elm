module VectorRacer.Ui.PanZoom exposing
    ( Msg
    , PanZoom
    , getTransform
    , getTransformString
    , init
    , subscriptions
    , update
    , withEvents
    )

import Browser.Events
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events.Extra.Wheel as Wheel
import Json.Decode
import Quantity exposing (Quantity)
import VectorRacer.Vector as Vector exposing (Vector)
import VectorRacer.Vector.Pixels as Pixels exposing (Pixels)



-- MODEL --


type PanZoom
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


type alias Offset =
    Vector Float Pixels


type alias Point =
    Vector Float Pixels


type alias Scale =
    Quantity Float Quantity.Unitless


init : PanZoom
init =
    Inactive
        { offset = Pixels.pixels ( 0, 0 )
        , scale = Quantity.float 1
        }



-- TODO: centerOn...


getTransform : PanZoom -> { offset : Offset, scale : Scale }
getTransform model =
    case model of
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
update msg model =
    case msg of
        MouseDown event ->
            let
                { offset, scale } =
                    getTransform model
            in
            MouseActive
                { baseOffset = offset
                , baseScale = scale
                , down = Pixels.pixels event.clientPos
                , current = Pixels.pixels event.clientPos
                }

        MouseMove event ->
            case model of
                Inactive _ ->
                    model

                MouseActive mouseState ->
                    MouseActive { mouseState | current = Pixels.pixels event.clientPos }

                TouchActive _ ->
                    model

        MouseUp _ ->
            case model of
                Inactive _ ->
                    model

                MouseActive _ ->
                    Inactive (getTransform model)

                TouchActive _ ->
                    model

        TouchStart event ->
            resetTouches event model

        TouchMove event ->
            case model of
                Inactive _ ->
                    model

                MouseActive _ ->
                    model

                TouchActive touchState ->
                    TouchActive
                        { touchState | touches = List.foldl updateTouches touchState.touches event.changedTouches }

        TouchEnd event ->
            endTouches event model

        TouchCancel event ->
            endTouches event model

        Wheel event ->
            case model of
                Inactive { offset, scale } ->
                    Inactive (updateZoom event offset scale)

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
                    model


endTouches : Touch.Event -> PanZoom -> PanZoom
endTouches event model =
    case model of
        Inactive _ ->
            model

        MouseActive _ ->
            model

        TouchActive _ ->
            if List.isEmpty event.touches then
                Inactive (getTransform model)

            else
                resetTouches event model


resetTouches : Touch.Event -> PanZoom -> PanZoom
resetTouches event model =
    let
        { offset, scale } =
            getTransform model
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


updateZoom : Wheel.Event -> Offset -> Scale -> { offset : Offset, scale : Scale }
updateZoom event offset scale =
    let
        zoom =
            wheelZoom event

        eventOffset =
            Pixels.pixels event.mouseEvent.clientPos

        offsetDiff =
            offset |> Vector.minus eventOffset

        offsetDiffZoomed =
            offsetDiff |> Vector.multiplyBy (Vector.fromFloat zoom)
    in
    { offset = offset |> Vector.plus offsetDiffZoomed |> Vector.minus offsetDiff
    , scale = Quantity.multiplyBy zoom scale
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



-- SUBSCRIPTIONS --


subscriptions : PanZoom -> Sub Msg
subscriptions model =
    case model of
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
    (Mouse.onWithOptions
        "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        MouseDown
        |> Html.Attributes.map toMsg
    )
        :: (Touch.onStart TouchStart |> Html.Attributes.map toMsg)
        :: (Touch.onMove TouchMove |> Html.Attributes.map toMsg)
        :: (Touch.onEnd TouchEnd |> Html.Attributes.map toMsg)
        :: (Touch.onCancel TouchCancel |> Html.Attributes.map toMsg)
        :: (Wheel.onWheel Wheel |> Html.Attributes.map toMsg)
        :: attributes
