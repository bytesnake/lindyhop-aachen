module Routes exposing (Route(..), toRelativeUrl, toRoute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, s, string, top)


type Route
    = Overview
    | Event String
    | Location String
    | NotFound


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (Parser.parse routeParser url)


toRelativeUrl : Route -> String
toRelativeUrl route =
    let
        parts =
            case route of
                Overview ->
                    []

                Event id ->
                    [ "event", id ]

                Location id ->
                    [ "location", id ]

                NotFound ->
                    []
    in
    "/" ++ String.join "/" (root ++ parts)


root : List String
root =
    [ "admin" ]


routeParser : Parser (Route -> a) a
routeParser =
    let
        rootUrl =
            List.map s root
                |> List.foldl (\next currentPath -> currentPath </> next) top
    in
    Parser.oneOf
        [ map Overview rootUrl
        , map Event (rootUrl </> s "event" </> string)
        , map Location (rootUrl </> s "location" </> string)
        ]
