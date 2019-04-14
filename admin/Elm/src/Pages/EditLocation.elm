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

import Css exposing (row)
import Events exposing (Event, Events, Location, Occurrence)
import Html.Styled exposing (Html, a, div, input, label, li, ol, p, text, textarea)
import Html.Styled.Attributes exposing (css, href, type_, value)
import Html.Styled.Events exposing (onInput)
import Http
import IdDict exposing (Id)
import Json.Encode as Encode
import List.Extra as List
import Pages.Utils as Utils exposing (viewDateTimeInput, viewInputNumber, viewInputText, viewTextArea)
import Parser
import Routes
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


fromEvents : String -> Events.Store -> Maybe Model
fromEvents rawId store =
    let
        locations =
            Events.locations store
    in
    IdDict.validate rawId locations
        |> Maybe.map
            (\id ->
                Model id (IdDict.get id locations)
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
    | InputAddress String
    | ClickedSave
    | SaveFinished (Result Http.Error ())
    | ClickedDelete
    | DeleteFinished (Result Http.Error Location)


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

        ClickedSave ->
            ( model, Events.updateLocation model.locationId model.location SaveFinished )

        SaveFinished result ->
            ( model, Cmd.none )

        ClickedDelete ->
            ( model, Events.deleteLocation model.locationId DeleteFinished )

        DeleteFinished result ->
            ( model, Cmd.none )


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
    [ Utils.breadcrumbs [ Routes.Overview ] (Routes.Location <| IdDict.encodeIdForUrl model.locationId)
    , Utils.fields
        [ viewInputText "Bezeichnung" model.location.name InputName
        , viewTextArea "Adresse" model.location.address InputAddress
        ]
    , div [ css [ Css.displayFlex, Css.flexDirection row ] ]
        [ Utils.button "Speichern" ClickedSave
        , Utils.button "Löschen" ClickedDelete
        ]
    ]
