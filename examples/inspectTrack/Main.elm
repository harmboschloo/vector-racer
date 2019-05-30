module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element
import Html
import Http
import Task
import VectorRacer.Grid as Grid
import VectorRacer.Track as Track exposing (Track)
import VectorRacer.Ui.TrackPanel as TrackPanel exposing (TrackPanel)
import VectorRacer.Vector.Pixels as Pixels



-- INIT --


type alias Flags =
    ()


type Model
    = LoadingViewport
    | LoadingTrack WindowSize
    | LoadError String
    | Loaded LoadedModel


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias LoadedModel =
    { track : Track
    , trackPanel : TrackPanel
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
    | GotTrackPanelMsg TrackPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( LoadingViewport, GotViewport { viewport } ) ->
            ( LoadingTrack
                { width = round viewport.width
                , height = round viewport.height
                }
            , fetchTrack
            )

        ( LoadingTrack _, GotWindowResize width height ) ->
            ( LoadingTrack { width = width, height = height }, Cmd.none )

        ( LoadingTrack windowSize, GotTrack trackImage (Ok (Ok track)) ) ->
            ( Loaded
                { track = track
                , trackPanel =
                    { panelSize = Pixels.pixels ( windowSize.width, windowSize.height )
                    , trackSize = Track.getSize track
                    , trackImage = trackImage
                    }
                        |> TrackPanel.init
                        |> TrackPanel.setGrid
                            (track
                                |> Track.getStartPositions
                                |> List.head
                                |> Maybe.map
                                    (\startPoint ->
                                        Grid.init
                                            { anchorPoint = startPoint
                                            , spacing = Pixels.pixels ( 15, 15 )
                                            }
                                    )
                            )
                }
            , Cmd.none
            )

        ( LoadingTrack _, GotTrack _ (Ok (Err trackError)) ) ->
            ( LoadError (Debug.toString trackError), Cmd.none )

        ( LoadingTrack _, GotTrack _ (Err httpError) ) ->
            ( LoadError (Debug.toString httpError), Cmd.none )

        ( Loaded loadedModel, GotWindowResize width height ) ->
            ( Loaded
                { loadedModel
                    | trackPanel =
                        TrackPanel.setPanelSize
                            (Pixels.pixels ( width, height ))
                            loadedModel.trackPanel
                }
            , Cmd.none
            )

        ( Loaded loadedModel, GotTrackPanelMsg trackPanelMsg ) ->
            ( Loaded
                { loadedModel | trackPanel = TrackPanel.update trackPanelMsg loadedModel.trackPanel }
            , Cmd.none
            )

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
            Sub.batch
                [ Browser.Events.onResize GotWindowResize
                , TrackPanel.subscriptions loadedModel.trackPanel |> Sub.map GotTrackPanelMsg
                ]



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

            Loaded { trackPanel } ->
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.clip
                    ]
                    (TrackPanel.view trackPanel |> Element.map GotTrackPanelMsg)
                ]
    }



-- MAIN --


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
