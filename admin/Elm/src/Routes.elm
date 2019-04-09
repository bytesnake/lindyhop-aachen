module Routes exposing (Route(..), toRoute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, s, string, top)


type Route
    = Overview
    | Event String
    | Location String
    | NotFound


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (Parser.parse route url)


pathPrefix : List String
pathPrefix =
    [ "admin" ]


route : Parser (Route -> a) a
route =
    let
        root =
            List.map s pathPrefix
                |> List.foldl (\next currentPath -> currentPath </> next) top
    in
    Parser.oneOf
        [ map Overview root
        , map Event (root </> s "event" </> string)
        , map Location (root </> s "location" </> string)
        ]
