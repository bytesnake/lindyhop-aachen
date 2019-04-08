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
import Html exposing (Html, input, label, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Http
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName newName ->
            let
                event =
                    model.event

                newEvent =
                    { event | name = newName }
            in
            ( { model | event = newEvent }, Cmd.none )


view : Model -> List (Html Msg)
view model =
    [ label []
        [ text "Titel"
        , input [ type_ "text", value model.event.name, onInput InputName ] []
        ]
    ]
