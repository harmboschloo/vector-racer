module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Http
import Quantity
import Quantity.Interval as Interval
import Svg
import Svg.Attributes
import Task
import VectorRacer.Grid as Grid exposing (Grid)
import VectorRacer.Track as Track exposing (Track)
import VectorRacer.Ui as Ui
import VectorRacer.Ui.PanZoom as PanZoom exposing (PanZoom)
import VectorRacer.Vector as Vector exposing (Vector)
import VectorRacer.Vector.Pixels as Pixels exposing (Pixels)



-- INIT --


type alias Flags =
    ()


type Model
    = LoadingViewport
    | LoadingTrack WindowSize
    | LoadError String
    | Loaded LoadedModel


type alias WindowSize =
    Vector Float Pixels


type alias LoadedModel =
    { track : Track
    , trackImage : String
    , panZoom : PanZoom
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( LoadingViewport
    , Task.perform GotViewport Browser.Dom.getViewport
    )


fetchTrack : Cmd Msg
fetchTrack =
    Http.get
        { url = "example.json"
        , expect = Http.expectJson (GotTrack "example.png") Track.decoder
        }



-- UPDATE --


type Msg
    = GotViewport Browser.Dom.Viewport
    | GotWindowResize Int Int
    | GotTrack String (Result Http.Error Track.DecodeResult)
    | GotPanZoomMsg PanZoom.Msg
    | GotTrackMouseMove Mouse.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( LoadingViewport, GotViewport { viewport } ) ->
            ( LoadingTrack (Pixels.pixels ( viewport.width, viewport.height ))
            , fetchTrack
            )

        ( LoadingTrack _, GotWindowResize width height ) ->
            ( LoadingTrack (Pixels.pixels ( toFloat width, toFloat height )), Cmd.none )

        ( LoadingTrack windowSize, GotTrack trackImage (Ok (Ok track)) ) ->
            ( Loaded
                { track = track
                , trackImage = trackImage
                , panZoom =
                    PanZoom.init
                        |> PanZoom.withScaleBounds (Interval.from (Quantity.float 0.5) (Quantity.float 2.5))
                        |> PanZoom.withOffset
                            (windowSize
                                |> Vector.minus (Track.getSize track |> Vector.toFloatVector)
                                |> Vector.divideBy (Vector.fromFloat 2)
                            )
                        |> PanZoom.withScale
                            (Quantity.float 1)
                            (Vector.toFloatVector (Track.getSize track) |> Vector.divideBy (Vector.fromFloat 2))
                }
            , Cmd.none
            )

        ( LoadingTrack _, GotTrack _ (Ok (Err trackError)) ) ->
            ( LoadError (Debug.toString trackError), Cmd.none )

        ( LoadingTrack _, GotTrack _ (Err httpError) ) ->
            ( LoadError (Debug.toString httpError), Cmd.none )

        ( Loaded loadedModel, GotPanZoomMsg panZoomMsg ) ->
            ( Loaded { loadedModel | panZoom = PanZoom.update panZoomMsg loadedModel.panZoom }
            , Cmd.none
            )

        ( Loaded _, GotTrackMouseMove event ) ->
            let
                _ =
                    Debug.log "mouse" event
            in
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LoadingViewport ->
            Sub.none

        LoadingTrack _ ->
            Browser.Events.onResize GotWindowResize

        LoadError _ ->
            Sub.none

        Loaded loadedModel ->
            PanZoom.subscriptions loadedModel.panZoom |> Sub.map GotPanZoomMsg



-- VIEW --


view : Model -> Browser.Document Msg
view model =
    { title = "inspect track example"
    , body =
        case model of
            LoadingViewport ->
                [ Html.text "..." ]

            LoadingTrack _ ->
                [ Html.text "..." ]

            LoadError error ->
                [ Html.text "Error: ", Html.text error ]

            Loaded { track, trackImage, panZoom } ->
                let
                    transform =
                        PanZoom.getTransformString panZoom
                in
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.clip
                    ]
                    (Element.html <|
                        Svg.svg
                            ([ Svg.Attributes.width "100%"
                             , Svg.Attributes.height "100%"
                             ]
                                |> PanZoom.withEvents
                                |> List.map (Html.Attributes.map GotPanZoomMsg)
                            )
                            [ Svg.g
                                [ Svg.Attributes.transform transform
                                ]
                                [ Ui.trackImage trackImage (Track.getSize track)
                                , Ui.trackBorder (Track.getSize track)
                                ]
                            , case getGrid track of
                                Just grid ->
                                    Ui.gridWithTransform transform grid

                                Nothing ->
                                    Svg.g [] []
                            ]
                    )
                ]
    }


getGrid : Track -> Maybe Grid
getGrid track =
    track
        |> Track.getStartPositions
        |> List.head
        |> Maybe.map
            (\startPoint ->
                Grid.init
                    { anchorPoint = startPoint
                    , spacing = Pixels.pixels ( 15, 15 )
                    }
            )



-- MAIN --


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
