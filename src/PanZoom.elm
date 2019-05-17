module PanZoom exposing
    ( Msg
    , State
    , get
    , init
    , subscriptions
    , update
    , withEvents
    , withTransform
    )

import Browser.Events
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode



-- TYPES --


type State
    = Inactive InactiveState
    | MouseActive MouseState
    | TouchActive TouchState


type alias InactiveState =
    { offset : ( Float, Float )
    , scale : Float
    }


type alias MouseState =
    { offset : ( Float, Float )
    , scale : Float
    , down : ( Float, Float )
    , current : ( Float, Float )
    }


type alias TouchState =
    { offset : ( Float, Float )
    , scale : Float
    , touches : List TouchData
    }


type alias TouchData =
    { id : Int
    , start : ( Float, Float )
    , current : ( Float, Float )
    }


type Msg
    = MouseDown Mouse.Event
    | MouseMove Mouse.Event
    | MouseUp Mouse.Event
    | TouchStart Touch.Event
    | TouchMove Touch.Event
    | TouchEnd Touch.Event
    | TouchCancel Touch.Event



-- STATE --


init : { offset : ( Float, Float ), scale : Float } -> State
init =
    Inactive


update : Msg -> State -> State
update msg state =
    case msg of
        MouseDown event ->
            let
                { offset, scale } =
                    get state
            in
            MouseActive
                { offset = offset
                , scale = scale
                , down = event.clientPos
                , current = event.clientPos
                }

        MouseMove event ->
            case state of
                Inactive _ ->
                    state

                MouseActive mouseState ->
                    MouseActive { mouseState | current = event.clientPos }

                TouchActive _ ->
                    state

        MouseUp _ ->
            case state of
                Inactive _ ->
                    state

                MouseActive _ ->
                    Inactive (get state)

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
                Inactive (get state)

            else
                resetTouches event state


resetTouches : Touch.Event -> State -> State
resetTouches event state =
    let
        { offset, scale } =
            get state
    in
    TouchActive
        { offset = offset
        , scale = scale
        , touches =
            List.map
                (\touch ->
                    { id = touch.identifier
                    , start = touch.clientPos
                    , current = touch.clientPos
                    }
                )
                event.touches
        }


updateTouches : Touch.Touch -> List TouchData -> List TouchData
updateTouches touch touches =
    List.map
        (\touchData ->
            if touchData.id == touch.identifier then
                { touchData | current = touch.clientPos }

            else
                touchData
        )
        touches


subscriptions : State -> Sub Msg
subscriptions state =
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
    Html.Attributes.map toMsg
        (Mouse.onWithOptions
            "mousedown"
            { stopPropagation = True
            , preventDefault = True
            }
            MouseDown
        )
        :: Html.Attributes.map toMsg (Touch.onStart TouchStart)
        :: Html.Attributes.map toMsg (Touch.onMove TouchMove)
        :: Html.Attributes.map toMsg (Touch.onEnd TouchEnd)
        :: Html.Attributes.map toMsg (Touch.onCancel TouchCancel)
        :: attributes


withTransform : State -> List (Html.Attribute msg) -> List (Html.Attribute msg)
withTransform state attributes =
    let
        { offset, scale } =
            get state

        ( dx, dy ) =
            offset
    in
    Html.Attributes.style
        "transform"
        ("translate("
            ++ String.fromFloat dx
            ++ "px,"
            ++ String.fromFloat dy
            ++ "px) scale("
            ++ String.fromFloat scale
            ++ ")"
        )
        :: Html.Attributes.style "transform-origin" "0 0"
        :: attributes



-- HELPERS --


get : State -> { offset : ( Float, Float ), scale : Float }
get state =
    case state of
        Inactive { offset, scale } ->
            { offset = offset, scale = scale }

        MouseActive { offset, scale, down, current } ->
            { offset = offset |> plus current |> minus down
            , scale = scale
            }

        TouchActive { offset, scale, touches } ->
            case touches of
                touchA :: touchB :: _ ->
                    let
                        distance0 =
                            distance touchA.start touchB.start

                        distance1 =
                            distance touchA.current touchB.current

                        zoom =
                            distance1 / distance0

                        mean0 =
                            mean touchA.start touchB.start

                        mean1 =
                            mean touchA.current touchB.current
                    in
                    { offset = mean1 |> minus (mean0 |> minus offset |> times ( zoom, zoom ))
                    , scale = scale * zoom
                    }

                touch :: _ ->
                    { offset = offset |> plus (touch.current |> minus touch.start)
                    , scale = scale
                    }

                _ ->
                    { offset = offset, scale = scale }


plus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
plus ( x1, y1 ) ( x2, y2 ) =
    ( x2 + x1, y2 + y1 )


minus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
minus ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1, y2 - y1 )


times : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
times ( x1, y1 ) ( x2, y2 ) =
    ( x2 * x1, y2 * y1 )


mean : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
mean ( x1, y1 ) ( x2, y2 ) =
    ( (x1 + x2) / 2, (y1 + y2) / 2 )


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
