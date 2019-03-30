module Main exposing (main)

import Browser
import Browser.Navigation as Browser
import Html exposing (Html, h1, text)
import Url exposing (Url)



-- Main


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }



-- I/O


init : () -> Url -> Browser.Key -> ( Model, Cmd Msg )
init _ url key =
    ( (), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Model


type alias Model =
    ()



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Lindy Hop Aachen Admin"
    , body =
        [ h1 [] [ text "Admin" ]
        ]
    }
