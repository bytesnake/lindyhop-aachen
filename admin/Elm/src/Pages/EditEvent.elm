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

import Events exposing (Event, Events, Id)
import Html exposing (Html, input, label, p, text, textarea)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Encode as Encode
import Session exposing (Session)


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
    , p [] [ text <| Encode.encode 2 (Events.encodeEvent model.event) ]
    ]


viewInputText : String -> String -> (String -> Msg) -> Html Msg
viewInputText lbl val inputMsg =
    label []
        [ text lbl
        , input [ type_ "text", value val, onInput inputMsg ] []
        ]


viewTextArea : String -> String -> (String -> Msg) -> Html Msg
viewTextArea lbl val inputMsg =
    label []
        [ text lbl
        , textarea [ value val, onInput inputMsg ] []
        ]
