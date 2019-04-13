module Pages.EditEvent exposing
    ( LoadModel
    , LoadMsg
    , Model
    , Msg
    , fromEvents
    , init
    , update
    , updateLoad
    , view
    )

import Events exposing (Event, Locations, Location, Occurrence)
import Html exposing (Html, a, input, label, li, ol, p, text, textarea)
import Html.Attributes exposing (href, type_, value)
import Html.Events exposing (onInput)
import Http
import IdDict exposing (Id)
import Json.Encode as Encode
import List.Extra as List
import Pages.Utils exposing (viewDateTimeInput, viewInputNumber, viewInputText, viewTextArea)
import Parser
import Time
import Utils.NaiveDateTime as Naive
import Utils.TimeFormat as TimeFormat


type alias Model =
    { eventId : Id Event
    , event : Event
    , locations : Locations
    }


type alias LoadModel =
    { rawId : String
    }


init : String -> ( LoadModel, Cmd LoadMsg )
init rawId =
    let
        fetchEvents =
            Events.fetchEvents FetchedEvents
    in
    ( LoadModel rawId, fetchEvents )


fromEvents : String -> Events.Store -> Maybe Model
fromEvents rawId store =
    let
        events =
            Events.events store

        locations = Events.locations store
    in
    IdDict.validate rawId events
        |> Maybe.map
            (\id ->
                Model id (IdDict.get id events) locations
            )


type LoadMsg
    = FetchedEvents (Result Http.Error Events.Store)


type LoadError
    = Http Http.Error
    | InvalidId String


updateLoad : LoadMsg -> LoadModel -> Result LoadError Model
updateLoad msg model =
    case msg of
        FetchedEvents result ->
            Result.mapError Http result
                |> Result.andThen
                    (\events ->
                        fromEvents model.rawId events
                            |> Result.fromMaybe (InvalidId model.rawId)
                    )


type Msg
    = InputName String
    | InputTeaser String
    | InputDescription String
    | InputOccurrence Int OccurrenceMsg


type OccurrenceMsg
    = InputStartDate String
    | InputStartTime String
    | InputDuration String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName newName ->
            let
                newModel =
                    updateEvent model
                        (\event -> { event | name = newName })
            in
            ( newModel, Cmd.none )

        InputTeaser newTeaser ->
            let
                newModel =
                    updateEvent model
                        (\event -> { event | teaser = newTeaser })
            in
            ( newModel, Cmd.none )

        InputDescription newDescription ->
            let
                newModel =
                    updateEvent model
                        (\event -> { event | description = newDescription })
            in
            ( newModel, Cmd.none )

        InputOccurrence index occurrenceMsg ->
            let
                newOccurrences occurrences =
                    List.updateAt index
                        (\occurrence ->
                            case occurrenceMsg of
                                InputDuration rawDuration ->
                                    case String.toInt rawDuration of
                                        Just newDuration ->
                                            { occurrence | duration = newDuration }

                                        Nothing ->
                                            occurrence

                                InputStartDate rawDate ->
                                    let
                                        newStart =
                                            case Parser.run Naive.dateParser rawDate of
                                                Ok date ->
                                                    Naive.setDate date occurrence.start

                                                Err _ ->
                                                    occurrence.start
                                    in
                                    { occurrence | start = newStart }

                                InputStartTime rawTime ->
                                    let
                                        newStart =
                                            case Parser.run Naive.timeParser rawTime of
                                                Ok time ->
                                                    Naive.setTime time occurrence.start

                                                Err _ ->
                                                    occurrence.start
                                    in
                                    { occurrence | start = newStart }
                        )
                        occurrences

                newModel =
                    updateEvent model
                        (\event ->
                            { event | occurrences = newOccurrences event.occurrences }
                        )
            in
            ( newModel, Cmd.none )


updateEvent : Model -> (Event -> Event) -> Model
updateEvent model eventUpdater =
    let
        event =
            model.event

        newEvent =
            eventUpdater event
    in
    { model | event = newEvent }


view : Model -> List (Html Msg)
view model =
    [ viewInputText "Titel" model.event.name InputName
    , viewInputText "Teaser" model.event.teaser InputTeaser
    , viewTextArea "Beschreibung" model.event.description InputDescription
    , ol []
        (List.indexedMap
            (\index occurrence ->
                li [] (viewEditOccurrence model.locations index occurrence)
            )
            model.event.occurrences
        )
    ]


viewEditOccurrence : Locations -> Int -> Occurrence -> List (Html Msg)
viewEditOccurrence locations index occurrence =
    let
        time =
            TimeFormat.time occurrence.start

        location = IdDict.get occurrence.locationId locations
    in
    [ viewDateTimeInput "Beginn" occurrence.start { dateChanged = InputOccurrence index << InputStartDate, timeChanged = InputOccurrence index << InputStartTime }
    , viewInputNumber "Dauer (in Minuten)" occurrence.duration (InputOccurrence index << InputDuration)
    , a [ href <| "../location/" ++ IdDict.encodeIdForUrl occurrence.locationId ] [ text location.name ]
    ]
