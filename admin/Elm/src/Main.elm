module Main exposing (main)

import Browser
import Browser.Navigation as Browser
import Events exposing (Event, Events, Location, Occurrence)
import Html exposing (Html, a, div, h1, h2, label, li, ol, p, text)
import Html.Attributes exposing (href, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Pages.EditEvent
import Pages.Overview
import Routes exposing (Route)
import Task
import Time
import Url exposing (Url)



-- Main


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- Model


type Model
    = Loaded Browser.Key RouteModel
    | Loading Browser.Key RouteModel RouteLoadModel


keyFromModel : Model -> Browser.Key
keyFromModel model =
    case model of
        Loaded key _ ->
            key

        Loading key _ _ ->
            key


loadedFromModel : Model -> RouteModel
loadedFromModel model =
    case model of
        Loaded _ routeModel ->
            routeModel

        Loading _ routeModel _ ->
            routeModel


type RouteModel
    = LoadingRoute
    | ErrorLoading
    | NotFound
    | Overview Pages.Overview.Model
    | EditEvent Pages.EditEvent.Model


type RouteLoadModel
    = OverviewLoad Pages.Overview.LoadModel
    | EditEventLoad Pages.EditEvent.LoadModel



-- I/O


init : () -> Url -> Browser.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Routes.toRoute url
    in
    initWith key route


initWith : Browser.Key -> Route -> ( Model, Cmd Msg )
initWith key route =
    load (Loaded key LoadingRoute) route


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OverviewLoadMsg Pages.Overview.LoadMsg
    | EditEventLoadMsg Pages.EditEvent.LoadMsg
    | EditEventMsg Pages.EditEvent.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    let
                        key =
                            keyFromModel model
                    in
                    ( model, Browser.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.load href )

        UrlChanged url ->
            load model (Routes.toRoute url)

        OverviewLoadMsg subMsg ->
            case model of
                Loading key loaded (OverviewLoad subModel) ->
                    case Pages.Overview.updateLoad subMsg subModel of
                        Ok newSubModel ->
                            ( Loaded key (Overview newSubModel), Cmd.none )

                        Err error ->
                            ( Loaded key ErrorLoading, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditEventLoadMsg subMsg ->
            case model of
                Loading key loaded (EditEventLoad subModel) ->
                    case Pages.EditEvent.updateLoad subMsg subModel of
                        Ok newSubModel ->
                            ( Loaded key (EditEvent newSubModel), Cmd.none )

                        Err error ->
                            ( Loaded key ErrorLoading, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditEventMsg subMsg ->
            let
                udpater routeModel =
                    case routeModel of
                        EditEvent subModel ->
                            Pages.EditEvent.update subMsg subModel
                                |> Tuple.mapBoth EditEvent (Cmd.map EditEventMsg)

                        _ ->
                            ( routeModel, Cmd.none )
            in
            updateLoaded udpater model


load : Model -> Route -> ( Model, Cmd Msg )
load model route =
    let
        key =
            keyFromModel model
    in
    case route of
        Routes.NotFound ->
            ( Loaded key <| NotFound, Cmd.none )

        Routes.Overview ->
            Pages.Overview.init OverviewLoadMsg
                |> wrapLoadModel model OverviewLoad

        Routes.Event rawId ->
            Pages.EditEvent.init rawId EditEventLoadMsg
                |> wrapLoadModel model EditEventLoad


wrapLoadModel : Model -> (subModel -> RouteLoadModel) -> ( subModel, Cmd msg ) -> ( Model, Cmd msg )
wrapLoadModel model wrapper updateTuple =
    let
        key =
            keyFromModel model

        loaded =
            loadedFromModel model
    in
    Tuple.mapFirst (\subModel -> Loading key loaded (wrapper subModel)) updateTuple


updateLoaded : (RouteModel -> ( RouteModel, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
updateLoaded updater model =
    case model of
        Loaded key loaded ->
            updater loaded |> Tuple.mapFirst (Loaded key)

        Loading key loaded loading ->
            updater loaded |> Tuple.mapFirst (\newLoaded -> Loading key newLoaded loading)



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Lindy Hop Aachen Admin"
    , body =
        case loadedFromModel model of
            LoadingRoute ->
                viewLoading

            ErrorLoading ->
                viewErrorLoading

            NotFound ->
                viewNotFound

            Overview subModel ->
                Pages.Overview.view subModel
                    |> List.map (Html.map (\_ -> NoOp))

            EditEvent subModel ->
                Pages.EditEvent.view subModel
                    |> List.map (Html.map EditEventMsg)
    }


viewLoading : List (Html Msg)
viewLoading =
    [ text "Loading..." ]


viewErrorLoading : List (Html Msg)
viewErrorLoading =
    [ text "There was an error while loading the app." ]


viewNotFound : List (Html Msg)
viewNotFound =
    [ text "Not found." ]
