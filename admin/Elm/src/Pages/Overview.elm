module Pages.Overview exposing
    ( LoadModel
    , LoadMsg
    , Model
    , init
    , updateLoad
    , view
    )

import Css exposing (em, inherit, none, zero)
import Css.Global as Css
import Events exposing (Event, Events, Location, Occurrence)
import Html.Styled exposing (Html, a, div, h1, h2, li, ol, text)
import Html.Styled.Attributes exposing (css, href)
import Http
import Time
import Utils.TimeFormat as TimeFormat


type alias Model =
    { events : Events
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
    , ol [ css [ listStyle, spreadListItemStyle ] ]
        (Events.map
            (\( id, event ) ->
                li []
                    [ a [ href <| "event/" ++ Events.stringFromId id, css [ hiddenLinkStyle ] ]
                        [ viewEvent event ]
                    ]
            )
            model.events
        )
    , h2 [] [ text "Orte" ]
    , ol [ css [ listStyle, spreadListItemStyle ] ]
        (List.map
            (\( id, location ) ->
                li []
                    [ a [ href <| "location/" ++ Events.stringFromId id, css [ hiddenLinkStyle ] ]
                        [ viewLocation location ]
                    ]
            )
            (Events.locations model.events)
        )
    ]


hiddenLinkStyle : Css.Style
hiddenLinkStyle =
    Css.batch
        [ Css.color inherit
        , Css.textDecoration inherit
        , Css.hover
            [ Css.color (Css.rgba 0 0 0 0.6)
            ]
        ]


listStyle : Css.Style
listStyle =
    Css.batch
        [ Css.listStyleType none
        , Css.padding zero
        ]


spreadListItemStyle : Css.Style
spreadListItemStyle =
    Css.batch
        [ Css.children
            [ Css.typeSelector "li"
                [ Css.adjacentSiblings
                    [ Css.typeSelector
                        "li"
                        [ Css.marginTop (em 0.5)
                        ]
                    ]
                ]
            ]
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
        , ol [ css [ listStyle ] ] listItems
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
