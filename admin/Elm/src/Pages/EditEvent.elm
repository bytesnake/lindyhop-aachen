module Pages.EditEvent exposing
    ( LoadModel
    , LoadMsg
    , Model
    , Msg
    , fromEvents
    , init
    , sessionFromModel
    , update
    , updateLoad
    , view
    )

import Events exposing (Event, Events, Id, Location, Occurrence)
import Html exposing (Html, input, label, li, ol, p, text, textarea)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Encode as Encode
import List.Extra as List
import Session exposing (Session)
import Time
import Utils.TimeFormat as TimeFormat


type alias Model =
    { session : Session
    , eventId : Id Event
    , event : Event
    }


sessionFromModel : Model -> Session
sessionFromModel model =
    model.session


type alias LoadModel =
    { session : Session
    , rawId : String
    }


init : Session -> String -> (LoadMsg -> msg) -> ( LoadModel, Cmd msg )
init session rawId toMsg =
    let
        fetchEvents =
            Events.fetchEvents FetchedEvents
    in
    ( LoadModel session rawId, Cmd.map toMsg fetchEvents )


fromEvents : Session -> String -> Events -> Maybe Model
fromEvents session rawId events =
    Events.findEvent rawId events
        |> Maybe.map (\( id, event ) -> Model session id event)


type LoadMsg
    = FetchedEvents (Result Http.Error Events)


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
                        fromEvents model.session model.rawId events
                            |> Result.fromMaybe (InvalidId model.rawId)
                    )


type Msg
    = InputName String
    | InputTeaser String
    | InputDescription String
    | InputOccurrenceDuration Int String


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

        InputOccurrenceDuration index rawDuration ->
            case String.toInt rawDuration of
                Just newDuration ->
                    let
                        updateOccurrence idx map occurrences =
                            List.updateAt idx map occurrences

                        newModel =
                            updateEvent model
                                (\event ->
                                    { event
                                        | occurrences =
                                            updateOccurrence index
                                                (\occurrence -> { occurrence | duration = newDuration })
                                                event.occurrences
                                    }
                                )
                    in
                    ( newModel, Cmd.none )

                Nothing ->
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
    [ viewInputText "Titel" model.event.name InputName
    , viewInputText "Teaser" model.event.teaser InputTeaser
    , viewTextArea "Beschreibung" model.event.description InputDescription
    , ol []
        (List.indexedMap
            (\index occurrence ->
                li [] (viewEditOccurrence model.session.timezone index occurrence)
            )
            model.event.occurrences
        )
    , p [] [ text <| Encode.encode 2 (Events.encodeEvent model.event) ]
    ]


viewEditOccurrence : Time.Zone -> Int -> Occurrence -> List (Html Msg)
viewEditOccurrence timezone index occurrence =
    let
        time =
            TimeFormat.stringFromPosix timezone occurrence.start

        ( _, location ) =
            occurrence.location
    in
    [ text time
    , viewInputNumber "Dauer (in Minuten)" occurrence.duration (InputOccurrenceDuration index)
    , text location.name
    ]



-- Utils


viewInputText : String -> String -> (String -> Msg) -> Html Msg
viewInputText lbl val inputMsg =
    label []
        [ text lbl
        , input [ type_ "text", value val, onInput inputMsg ] []
        ]


viewInputNumber : String -> Int -> (String -> Msg) -> Html Msg
viewInputNumber lbl val inputMsg =
    label []
        [ text lbl
        , input [ type_ "number", value <| String.fromInt val, onInput inputMsg ] []
        ]


viewTextArea : String -> String -> (String -> Msg) -> Html Msg
viewTextArea lbl val inputMsg =
    label []
        [ text lbl
        , textarea [ value val, onInput inputMsg ] []
        ]
