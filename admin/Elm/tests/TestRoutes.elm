module TestRoutes exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Routes
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "routes toUrl and toRoute returns same route"
        (List.map
            (\route ->
                test ("for route " ++ Routes.toRelativeUrl route) <|
                    \_ ->
                        Routes.toRelativeUrl route
                            |> exampleUrlFromString
                            |> Routes.toRoute
                            |> Expect.equal route
            )
            [ Routes.Overview, Routes.Event "1", Routes.Location "1" ]
        )


exampleUrlFromString : String -> Url
exampleUrlFromString path =
    { protocol = Url.Https
    , host = "example.com"
    , port_ = Nothing
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
