module Pages.Overview exposing
    ( LoadModel
    , LoadMsg
    , Model
    , init
    , updateLoad
    , view
    )

import Events exposing (Event, Events, Location, Occurrence)
import Html exposing (Html, a, div, h1, h2, li, ol, text)
import Html.Attributes exposing (href)
import Http
import Time
import Utils.TimeFormat as TimeFormat


type alias Model =
    { events : Events
    }


type alias LoadModel =
    {}


init : (LoadMsg -> msg) -> ( LoadModel, Cmd msg )
init toMsg =
    let
        fetchEvents =
            Events.fetchEvents FetchedEvents
    in
    ( LoadModel, Cmd.map toMsg fetchEvents )


type LoadMsg
    = FetchedEvents (Result Http.Error Events)


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
        (Events.map
            (\( id, event ) ->
                li []
                    [ a [ href <| "event/" ++ Events.stringFromId id ]
                        [ viewEvent event ]
                    ]
            )
            model.events
        )
    , h2 [] [ text "Orte" ]
    , ol []
        (List.map
            (\( id, location ) ->
                li []
                    [ a [ href <| "location/" ++ Events.stringFromId id ]
                        [ viewLocation location ]
                    ]
            )
            (Events.locations model.events)
        )
    ]


viewEvent : Event -> Html msg
viewEvent event =
    let
        max =
            5

        occurrencesPreview =
            List.take max event.occurrences

        doesOverflow =
            List.length event.occurrences > max

        occurrenceListItems =
            List.map (\occurrence -> li [] [ viewOccurrence occurrence ]) occurrencesPreview

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


viewOccurrence : Occurrence -> Html msg
viewOccurrence occurrence =
    let
        location =
            Tuple.second occurrence.location
    in
    div []
        [ text <| TimeFormat.fullDate occurrence.start ++ " - " ++ location.name ]


viewLocation : Location -> Html msg
viewLocation location =
    div []
        [ text <| location.name ++ " (" ++ location.address ++ ")"
        ]
