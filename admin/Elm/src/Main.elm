module Main exposing (main)

import Browser
import Browser.Navigation as Browser
import Date exposing (Date)
import Events exposing (Event, Events, Location, Occurrence)
import Html exposing (Html, a, div, h1, h2, li, ol, p, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Routes exposing (Route)
import Task
import Time
import Url exposing (Url)
import Utils.SimpleTime as SimpleTime exposing (SimpleTime)



-- Main


main =
    Browser.application
        { init = init
        , view = view
        , update = appUpdate
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- Model


{-| The highest level model.
-}
type alias AppModel =
    { key : Browser.Key
    , status : AppModelStatus
    }


{-| Describes the app startup.
-}
type AppModelStatus
    = Loading Route
    | LoadedEvents Route Events
    | ErrorLoadingEvents Http.Error
    | LoadedTimezone Route Time.Zone
    | Loaded Model


{-| The model for the loaded app.
-}
type alias Model =
    { common : Common
    , route : RouteModel
    }


{-| Data that is shared between all routes.
-}
type alias Common =
    { timezone : Time.Zone
    , events : Events
    }


{-| Each route's model.
-}
type RouteModel
    = Overview
    | Event Event
    | NotFound



-- I/O


init : () -> Url -> Browser.Key -> ( AppModel, Cmd AppMsg )
init _ url key =
    let
        route =
            Routes.toRoute url
    in
    initWith key route


initWith : Browser.Key -> Route -> ( AppModel, Cmd AppMsg )
initWith key route =
    let
        getTimezone =
            Task.perform FetchedTimezone Time.here

        getEvents =
            Events.fetchEvents FetchedEvents
    in
    ( AppModel key (Loading route), Cmd.batch [ getTimezone, getEvents ] )


subscriptions : AppModel -> Sub AppMsg
subscriptions model =
    Sub.none



-- Update


type AppMsg
    = AppNoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | FetchedTimezone Time.Zone
    | FetchedEvents (Result Http.Error Events)
    | SubMsg Msg


appUpdate : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
appUpdate msg model =
    case msg of
        AppNoOp ->
            ( model, Cmd.none )

        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.load href )

        UrlChanged url ->
            let
                route =
                    Routes.toRoute url

                modelStatus =
                    case model.status of
                        Loading _ ->
                            Loading route

                        LoadedEvents _ events ->
                            LoadedEvents route events

                        LoadedTimezone _ zone ->
                            LoadedTimezone route zone

                        ErrorLoadingEvents _ ->
                            model.status

                        Loaded subModel ->
                            let
                                subModelRoute =
                                    case route of
                                        Routes.Overview ->
                                            Overview

                                        Routes.Event rawId ->
                                            case Events.findEvent rawId subModel.common.events of
                                                Just ( _, event ) ->
                                                    Event event

                                                Nothing ->
                                                    NotFound

                                        Routes.NotFound ->
                                            NotFound
                            in
                            Loaded (Model subModel.common subModelRoute)
            in
            ( AppModel model.key modelStatus, Cmd.none )

        FetchedTimezone zone ->
            let
                newModel =
                    load (Just zone) Nothing model
            in
            ( newModel, Cmd.none )

        FetchedEvents result ->
            let
                newModel =
                    case result of
                        Ok events ->
                            load Nothing (Just events) model

                        Err error ->
                            AppModel model.key <| ErrorLoadingEvents error
            in
            ( newModel, Cmd.none )

        SubMsg subMsg ->
            case model.status of
                Loaded loadedModel ->
                    update subMsg loadedModel
                        |> Tuple.mapBoth (AppModel model.key << Loaded) (Cmd.map SubMsg)

                _ ->
                    ( model, Cmd.none )


load : Maybe Time.Zone -> Maybe Events -> AppModel -> AppModel
load maybeZone maybeEvents model =
    let
        wrap : AppModelStatus -> AppModel
        wrap status =
            AppModel model.key status

        loaded : Route -> Time.Zone -> Events -> AppModelStatus
        loaded route zone events =
            let
                routeModel =
                    case route of
                        Routes.Overview ->
                            Overview

                        Routes.Event id ->
                            Events.findEvent id events
                                |> Maybe.map (\( eventId, event ) -> Event event)
                                |> Maybe.withDefault NotFound

                        Routes.NotFound ->
                            NotFound
            in
            Loaded <| Model (Common zone events) routeModel
    in
    case model.status of
        Loading route ->
            (case ( maybeZone, maybeEvents ) of
                ( Just zone, Just events ) ->
                    loaded route zone events

                ( Just zone, Nothing ) ->
                    LoadedTimezone route zone

                ( Nothing, Just events ) ->
                    LoadedEvents route events

                ( Nothing, Nothing ) ->
                    Loading route
            )
                |> wrap

        LoadedEvents route events ->
            (case maybeZone of
                Just zone ->
                    loaded route zone events

                Nothing ->
                    LoadedEvents route events
            )
                |> wrap

        LoadedTimezone route zone ->
            (case maybeEvents of
                Just events ->
                    loaded route zone events

                Nothing ->
                    LoadedTimezone route zone
            )
                |> wrap

        ErrorLoadingEvents _ ->
            model

        Loaded _ ->
            model


type Msg
    = NoOp
    | EventSelected Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EventSelected event ->
            ( Model model.common <| Event event, Cmd.none )



-- View


view : AppModel -> Browser.Document AppMsg
view appModel =
    let
        html : List (Html AppMsg)
        html =
            case appModel.status of
                Loading _ ->
                    viewLoading

                LoadedEvents _ _ ->
                    viewLoading

                LoadedTimezone _ _ ->
                    viewLoading

                ErrorLoadingEvents error ->
                    viewError error

                Loaded model ->
                    viewLoaded model
                        |> List.map (Html.map SubMsg)
    in
    { title = "Lindy Hop Aachen Admin"
    , body = html
    }


viewLoading : List (Html AppMsg)
viewLoading =
    [ h1 [] [ text "Admin" ]
    , p [] [ text "Lädt..." ]
    ]


viewError : Http.Error -> List (Html AppMsg)
viewError error =
    let
        errorMessage =
            case error of
                Http.BadUrl url ->
                    "Es gibt einen Programmierfehler. (Die URL ist nicht wohlgeformt.)"

                Http.Timeout ->
                    "Der Server hat zu lange gebraucht, um eine Antwort zu senden."

                Http.NetworkError ->
                    "Der Server ist nicht erreichbar."

                Http.BadStatus status ->
                    "Es gibt einen Programmierfehler. (Der Server antwortet mit Statuscode " ++ String.fromInt status ++ ".)"

                Http.BadBody err ->
                    "Der Server hat ungültig geantwortet. (" ++ err ++ ")"
    in
    [ h1 [] [ text "Admin" ]
    , p []
        [ text "Beim Laden der Events ist ein Fehler passiert:"
        , Html.br [] []
        , text errorMessage
        ]
    ]


viewLoaded : Model -> List (Html Msg)
viewLoaded model =
    case model.route of
        Overview ->
            viewOverview model.common.timezone model.common.events

        Event event ->
            viewEventEdit model.common.timezone event

        NotFound ->
            viewNotFound


viewOverview : Time.Zone -> Events -> List (Html Msg)
viewOverview zone events =
    [ h1 [] [ text "Admin" ]
    , h2 [] [ text "Veranstaltungen" ]
    , ol []
        (Events.map
            (\( id, event ) ->
                li []
                    [ a [ href <| "event/" ++ Events.stringFromId id ]
                        [ viewEvent zone event ]
                    ]
            )
            events
        )
    , h2 [] [ text "Orte" ]
    , ol []
        (List.map
            (\( id, location ) ->
                li []
                    [ a [ href <| "location/" ++ Events.stringFromId id ]
                        [ viewLocation location ]
                    ]
            )
            (Events.locations events)
        )
    ]


viewEventEdit : Time.Zone -> Event -> List (Html Msg)
viewEventEdit zone event =
    [ h1 [] [ text "Admin" ]
    , viewEvent zone event
    ]


viewEvent : Time.Zone -> Event -> Html Msg
viewEvent zone event =
    let
        max =
            5

        occurrencesPreview =
            List.take max event.occurrences

        doesOverflow =
            List.length event.occurrences > max

        occurrenceListItems =
            List.map (\( _, occurrence ) -> li [] [ viewOccurrence zone occurrence ]) occurrencesPreview

        listItems =
            occurrenceListItems
                ++ (if doesOverflow then
                        [ li [] [ text "…" ] ]

                    else
                        []
                   )
    in
    div []
        [ text event.name
        , ol [] listItems
        ]


viewOccurrence : Time.Zone -> Occurrence -> Html Msg
viewOccurrence zone occurrence =
    let
        location =
            Tuple.second occurrence.location
    in
    div []
        [ text <| stringFromPosix zone occurrence.start ++ " - " ++ location.name ]


viewLocation : Location -> Html Msg
viewLocation location =
    div []
        [ text <| location.name ++ " (" ++ location.address ++ ")"
        ]


viewNotFound : List (Html Msg)
viewNotFound =
    [ h1 [] [ text "404 - Not Found" ] ]



-- View Helpers


stringFromPosix : Time.Zone -> Time.Posix -> String
stringFromPosix zone posix =
    let
        date =
            Date.fromPosix zone posix
                |> stringFromDate

        time =
            SimpleTime.fromPosix zone posix
                |> stringFromSimpleTime
    in
    date ++ " " ++ time


stringFromDate : Date -> String
stringFromDate date =
    let
        day =
            Date.day date
                |> padInt

        month =
            Date.monthNumber date
                |> padInt

        year =
            Date.year date
                |> String.fromInt
    in
    day ++ "." ++ month ++ "." ++ year


stringFromSimpleTime : SimpleTime -> String
stringFromSimpleTime time =
    let
        hour =
            SimpleTime.hour time
                |> padInt

        minute =
            SimpleTime.minute time
                |> padInt
    in
    hour ++ ":" ++ minute


padInt : Int -> String
padInt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n
