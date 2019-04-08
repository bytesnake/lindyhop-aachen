module Pages.Overview exposing
    ( LoadModel
    , LoadMsg
    , Model
    , init
    , sessionFromModel
    , updateLoad
    , view
    )

import Date exposing (Date)
import Events exposing (Event, Events, Location, Occurrence)
import Html exposing (Html, a, div, h1, h2, li, ol, text)
import Html.Attributes exposing (href)
import Http
import Session exposing (Session)
import Time
import Utils.SimpleTime as SimpleTime exposing (SimpleTime)


type alias Model =
    { session : Session
    , events : Events
    }


type alias LoadModel =
    { session : Session }


sessionFromModel : Model -> Session
sessionFromModel model =
    model.session


init : Session -> (LoadMsg -> msg) -> ( LoadModel, Cmd msg )
init session toMsg =
    let
        fetchEvents =
            Events.fetchEvents FetchedEvents
    in
    ( LoadModel session, Cmd.map toMsg fetchEvents )


type LoadMsg
    = FetchedEvents (Result Http.Error Events)


updateLoad : LoadMsg -> LoadModel -> Result Http.Error Model
updateLoad msg model =
    case msg of
        FetchedEvents result ->
            Result.map (Model model.session) result


view : Model -> List (Html msg)
view model =
    [ h1 [] [ text "Admin" ]
    , h2 [] [ text "Veranstaltungen" ]
    , ol []
        (Events.map
            (\( id, event ) ->
                li []
                    [ a [ href <| "event/" ++ Events.stringFromId id ]
                        [ viewEvent model.session.timezone event ]
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


viewEvent : Time.Zone -> Event -> Html msg
viewEvent zone event =
    let
        max =
            5

        occurrencesPreview =
            List.take max event.occurrences

        doesOverflow =
            List.length event.occurrences > max

        occurrenceListItems =
            List.map (\occurrence -> li [] [ viewOccurrence zone occurrence ]) occurrencesPreview

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


viewOccurrence : Time.Zone -> Occurrence -> Html msg
viewOccurrence zone occurrence =
    let
        location =
            Tuple.second occurrence.location
    in
    div []
        [ text <| stringFromPosix zone occurrence.start ++ " - " ++ location.name ]


viewLocation : Location -> Html msg
viewLocation location =
    div []
        [ text <| location.name ++ " (" ++ location.address ++ ")"
        ]



-- Utils


stringFromPosix : Time.Zone -> Time.Posix -> String
stringFromPosix zone posix =
    let
        date =
            Date.fromPosix zone posix
                |> stringFromDate

        time =
            SimpleTime.fromPosix zone posix
                |> stringFromSimpleTime
    in
    date ++ " " ++ time


stringFromDate : Date -> String
stringFromDate date =
    let
        day =
            Date.day date
                |> padInt

        month =
            Date.monthNumber date
                |> padInt

        year =
            Date.year date
                |> String.fromInt
    in
    day ++ "." ++ month ++ "." ++ year


stringFromSimpleTime : SimpleTime -> String
stringFromSimpleTime time =
    let
        hour =
            SimpleTime.hour time
                |> padInt

        minute =
            SimpleTime.minute time
                |> padInt
    in
    hour ++ ":" ++ minute


padInt : Int -> String
padInt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n
