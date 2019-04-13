module Pages.Overview exposing
    ( LoadModel
    , LoadMsg
    , Model
    , init
    , updateLoad
    , view
    )

import Events exposing (Event, Location, Locations, Occurrence)
import Html exposing (Html, a, div, h1, h2, li, ol, text)
import Html.Attributes exposing (href)
import Http
import IdDict exposing (encodeIdForUrl)
import Time
import Utils.TimeFormat as TimeFormat


type alias Model =
    { events : Events.Store
    }


type alias LoadModel =
    {}


init : ( LoadModel, Cmd LoadMsg )
init =
    let
        fetchEvents =
            Events.fetchEvents FetchedEvents
    in
    ( LoadModel, fetchEvents )


type LoadMsg
    = FetchedEvents (Result Http.Error Events.Store)


updateLoad : LoadMsg -> LoadModel -> Result Http.Error Model
updateLoad msg model =
    case msg of
        FetchedEvents result ->
            Result.map Model result


view : Model -> List (Html msg)
view model =
    [ h1 [] [ text "Admin" ]
    , h2 [] [ text "Veranstaltungen" ]
    , ol []
        (Events.mapEvents
            (\id event ->
                li []
                    [ a [ href <| "event/" ++ encodeIdForUrl id ]
                        [ viewEvent (Events.locations model.events) event ]
                    ]
            )
            model.events
        )
    , h2 [] [ text "Orte" ]
    , ol []
        (Events.mapLocations
            (\id location ->
                li []
                    [ a [ href <| "location/" ++ encodeIdForUrl id ]
                        [ viewLocation location ]
                    ]
            )
            model.events
        )
    ]


viewEvent : Locations -> Event -> Html msg
viewEvent locations event =
    let
        max =
            5

        occurrencesPreview =
            List.take max event.occurrences

        doesOverflow =
            List.length event.occurrences > max

        occurrenceListItems =
            List.map (\occurrence -> li [] [ viewOccurrence locations occurrence ]) occurrencesPreview

        listItems =
            occurrenceListItems
                ++ (if doesOverflow then
                        [ li [] [ text "…" ] ]

                    else
                        []
                   )
    in
    div []
        [ text event.name
        , ol [] listItems
        ]


viewOccurrence : Locations -> Occurrence -> Html msg
viewOccurrence locations occurrence =
    let
        location =
            IdDict.get occurrence.locationId locations
    in
    div []
        [ text <| TimeFormat.fullDate occurrence.start ++ " - " ++ location.name ]


viewLocation : Location -> Html msg
viewLocation location =
    div []
        [ text <| location.name ++ " (" ++ location.address ++ ")"
        ]
