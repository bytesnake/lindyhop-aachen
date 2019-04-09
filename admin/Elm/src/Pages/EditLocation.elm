module Pages.EditLocation exposing
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

import Events exposing (Event, Events, Id, Location, Occurrence)
import Html exposing (Html, a, input, label, li, ol, p, text, textarea)
import Html.Attributes exposing (href, type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Encode as Encode
import List.Extra as List
import Pages.Utils exposing (viewDateTimeInput, viewInputNumber, viewInputText, viewTextArea)
import Parser
import Time
import Utils.NaiveDateTime as Naive
import Utils.TimeFormat as TimeFormat


type alias Model =
    { locationId : Id Location
    , location : Location
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


fromEvents : String -> Events -> Maybe Model
fromEvents rawId events =
    Events.findLocation rawId events
        |> Maybe.map (\( id, event ) -> Model id event)


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
                        fromEvents model.rawId events
                            |> Result.fromMaybe (InvalidId model.rawId)
                    )


type Msg
    = InputName String
    | InputAddress String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName newName ->
            let
                newModel =
                    updateLocation model
                        (\location -> { location | name = newName })
            in
            ( newModel, Cmd.none )

        InputAddress newAddress ->
            let
                newModel =
                    updateLocation model
                        (\location -> { location | address = newAddress })
            in
            ( newModel, Cmd.none )


updateLocation : Model -> (Location -> Location) -> Model
updateLocation model locationUpdater =
    let
        location =
            model.location

        newLocation =
            locationUpdater location
    in
    { model | location = newLocation }


view : Model -> List (Html Msg)
view model =
    [ viewInputText "Bezeichnung" model.location.name InputName
    , viewTextArea "Adresse" model.location.address InputAddress
    , p [] [ text <| Encode.encode 2 (Events.encodeLocation model.location) ]
    ]
