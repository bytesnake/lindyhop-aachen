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

import Css exposing (em, row, zero, flexStart)
import Css.Global as Css
import Events exposing (Event, Location, Locations, Occurrence)
import Html.Styled exposing (Html, a, div, h2, input, label, li, ol, p, text, textarea)
import Html.Styled.Attributes exposing (css, href, type_, value)
import Html.Styled.Events exposing (onInput)
import Http
import IdDict exposing (Id)
import Json.Encode as Encode
import List.Extra as List
import Pages.Utils as Utils exposing (fields, labeled, viewDateTimeInput, viewInputNumber, viewInputText, viewTextArea)
import Parser
import Routes
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

        locations =
            Events.locations store
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
    | ClickedSave
    | SaveFinished (Result Http.Error ())
    | ClickedDelete
    | DeleteFinished (Result Http.Error Event)


type OccurrenceMsg
    = InputStartDate String
    | InputStartTime String
    | InputDuration String
    | InputClickedDelete


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
                    let
                        doUpdate : (Occurrence -> Occurrence) -> List Occurrence
                        doUpdate updateMapping =
                            List.updateAt index
                                updateMapping
                                occurrences
                    in
                    case occurrenceMsg of
                        InputDuration rawDuration ->
                            case String.toInt rawDuration of
                                Just newDuration ->
                                    doUpdate (\occurrence -> { occurrence | duration = newDuration })

                                Nothing ->
                                    occurrences

                        InputStartDate rawDate ->
                            let
                                newStart occurrence =
                                    case Parser.run Naive.dateParser rawDate of
                                        Ok date ->
                                            Naive.setDate date occurrence.start

                                        Err _ ->
                                            occurrence.start
                            in
                            doUpdate (\occurrence -> { occurrence | start = newStart occurrence })

                        InputStartTime rawTime ->
                            let
                                newStart occurrence =
                                    case Parser.run Naive.timeParser rawTime of
                                        Ok time ->
                                            Naive.setTime time occurrence.start

                                        Err _ ->
                                            occurrence.start
                            in
                            doUpdate (\occurrence -> { occurrence | start = newStart occurrence })

                        InputClickedDelete ->
                            List.removeAt index occurrences

                newModel =
                    updateEvent model
                        (\event ->
                            { event | occurrences = newOccurrences event.occurrences }
                        )
            in
            ( newModel, Cmd.none )

        ClickedSave ->
            ( model, Events.updateEvent model.eventId model.event SaveFinished )

        SaveFinished result ->
            ( model, Cmd.none )

        ClickedDelete ->
            ( model, Events.deleteEvent model.locations model.eventId DeleteFinished )

        DeleteFinished result ->
            ( model, Cmd.none )


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
    [ Utils.breadcrumbs [ Routes.Overview ] (Routes.Event <| IdDict.encodeIdForUrl model.eventId)
    , fields
        [ viewInputText "Titel" model.event.name InputName
        , viewInputText "Teaser" model.event.teaser InputTeaser
        , viewTextArea "Beschreibung" model.event.description InputDescription
        ]
    , h2 [] [ text "Termine" ]
    , ol [ css [ spreadListItemStyle ] ]
        (List.indexedMap
            (\index occurrence ->
                li [] [ viewEditOccurrence model.locations index occurrence ]
            )
            model.event.occurrences
        )
    , div [ css [ Css.displayFlex, Css.flexDirection row ] ]
        [ Utils.button "Speichern" ClickedSave
        , Utils.button "Löschen" ClickedDelete
        ]
    ]


spreadListItemStyle : Css.Style
spreadListItemStyle =
    Css.batch
        [ Css.children
            [ Css.typeSelector "li"
                [ Css.adjacentSiblings
                    [ Css.typeSelector
                        "li"
                        [ Css.marginTop (em 1)
                        ]
                    ]
                ]
            ]
        ]


viewEditOccurrence : Locations -> Int -> Occurrence -> Html Msg
viewEditOccurrence locations index occurrence =
    let
        time =
            TimeFormat.time occurrence.start

        location =
            IdDict.get occurrence.locationId locations

        occMsg : OccurrenceMsg -> Msg
        occMsg subMsg =
            InputOccurrence index subMsg

        occurrenceStyle =
            Css.batch
                [ Css.displayFlex
                , Css.flexDirection row
                , Css.alignItems flexStart
                , Css.children
                    [ Css.everything
                        [ Css.adjacentSiblings
                            [ Css.everything
                                [ Css.marginLeft (em 1)
                                ]
                            ]
                        , Css.paddingTop zero
                        , Css.paddingBottom zero
                        ]
                    ]
                ]
    in
    div [ css [ occurrenceStyle ] ]
        [ viewDateTimeInput "Beginn"
            occurrence.start
            { dateChanged = occMsg << InputStartDate
            , timeChanged = occMsg << InputStartTime
            }
        , viewInputNumber "Dauer (in Minuten)" occurrence.duration (occMsg << InputDuration)
        , labeled "Ort" [ a [ href (Routes.toRelativeUrl <| Routes.Location <| IdDict.encodeIdForUrl occurrence.locationId) ] [ text location.name ] ]
        , Utils.button "Löschen" (occMsg InputClickedDelete)
        ]
