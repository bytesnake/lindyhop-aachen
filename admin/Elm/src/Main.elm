module Main exposing (main)

import Browser
import Browser.Navigation as Browser
import Events.Event as Event exposing (Event, FullEvent, fullEventDecoder)
import Events.Location as Location exposing (Location)
import Events.Occurrence as Occurrence exposing (Occurrence)
import Html exposing (Html, div, h1, h2, li, ol, p, text)
import Http
import Json.Decode as Decode
import Task
import Time
import Url exposing (Url)



-- Main


main =
    Browser.application
        { init = init
        , view = view
        , update = appUpdate
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> AppNoOp
        , onUrlChange = \_ -> AppNoOp
        }



-- Model


type AppModel
    = Loading
    | LoadedEvents (List FullEvent)
    | ErrorLoadingEvents Http.Error
    | LoadedTimezone Time.Zone
    | Loaded Model


type alias Model =
    { timezone : Time.Zone
    , events : List ( Occurrence, Event )
    }



-- I/O


init : () -> Url -> Browser.Key -> ( AppModel, Cmd AppMsg )
init _ url key =
    let
        getTimezone =
            Task.perform FetchedTimezone Time.here

        getEvents =
            Http.get
                { url = "/api/events"
                , expect = Http.expectJson FetchedEvents (Decode.list fullEventDecoder)
                }
    in
    ( Loading, Cmd.batch [ getTimezone, getEvents ] )


subscriptions : AppModel -> Sub AppMsg
subscriptions model =
    Sub.none



-- Update


type AppMsg
    = AppNoOp
    | FetchedTimezone Time.Zone
    | FetchedEvents (Result Http.Error (List FullEvent))
    | Sub Msg


appUpdate : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
appUpdate msg model =
    case msg of
        AppNoOp ->
            ( model, Cmd.none )

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
                            ErrorLoadingEvents error
            in
            ( newModel, Cmd.none )

        Sub subMsg ->
            case model of
                Loaded loadedModel ->
                    update subMsg loadedModel
                        |> Tuple.mapBoth Loaded (Cmd.map Sub)

                _ ->
                    ( model, Cmd.none )


load : Maybe Time.Zone -> Maybe (List FullEvent) -> AppModel -> AppModel
load maybeZone maybeEvents model =
    let
        loaded : Time.Zone -> List FullEvent -> AppModel
        loaded zone events =
            Loaded
                { timezone = zone
                , events = sortByOccurrences events
                }
    in
    case model of
        Loading ->
            case ( maybeZone, maybeEvents ) of
                ( Just zone, Just events ) ->
                    loaded zone events

                ( Just zone, Nothing ) ->
                    LoadedTimezone zone

                ( Nothing, Just events ) ->
                    LoadedEvents events

                ( Nothing, Nothing ) ->
                    Loading

        LoadedEvents events ->
            case maybeZone of
                Just zone ->
                    loaded zone events

                Nothing ->
                    LoadedEvents events

        LoadedTimezone zone ->
            case maybeEvents of
                Just events ->
                    loaded zone events

                Nothing ->
                    LoadedTimezone zone

        ErrorLoadingEvents _ ->
            model

        Loaded _ ->
            model


sortByOccurrences : List FullEvent -> List ( Occurrence, Event )
sortByOccurrences fullEvents =
    List.concatMap
        (\fullEvent ->
            let
                event =
                    Event.event fullEvent
            in
            List.map
                (\occurrence -> ( occurrence, event ))
                (Event.occurrences fullEvent)
        )
        fullEvents


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : AppModel -> Browser.Document AppMsg
view appModel =
    let
        html : List (Html AppMsg)
        html =
            case appModel of
                Loading ->
                    viewLoading

                LoadedEvents _ ->
                    viewLoading

                LoadedTimezone _ ->
                    viewLoading

                ErrorLoadingEvents error ->
                    viewError error

                Loaded model ->
                    viewLoaded model
                        |> List.map (Html.map Sub)
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
    [ h1 [] [ text "Admin" ]
    , p [] [ text "Beim Laden der Events ist ein Fehler passiert." ]
    ]


viewLoaded : Model -> List (Html Msg)
viewLoaded model =
    [ h1 [] [ text "Admin" ]
    , ol [] (List.map (\( occurrence, event ) -> li [] [ viewOccurrence model.timezone occurrence event ]) model.events)
    ]


viewOccurrence : Time.Zone -> Occurrence -> Event -> Html Msg
viewOccurrence zone occurrence event =
    let
        location =
            Occurrence.location occurrence

        description =
            (stringFromDate zone <| Occurrence.start occurrence)
                ++ " - "
                ++ Location.name location
    in
    div []
        [ h2 [] [ text (Event.name event) ]
        , p [] [ text description ]
        ]


stringFromDate : Time.Zone -> Time.Posix -> String
stringFromDate zone time =
    let
        day =
            Time.toDay zone time
                |> padInt

        month =
            case Time.toMonth zone time of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"

        year =
            Time.toYear zone time
                |> String.fromInt

        hour =
            Time.toHour zone time
                |> padInt

        minute =
            Time.toMinute zone time
                |> padInt
    in
    day ++ "." ++ month ++ "." ++ year ++ " " ++ hour ++ ":" ++ minute


padInt : Int -> String
padInt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n
