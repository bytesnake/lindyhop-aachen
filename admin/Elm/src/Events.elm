module Events exposing
    ( Event
    , EventList
    , Id
    , Location
    , Occurrence
    , decodeEventList
    , fetchEvents
    , findEvent
    , stringFromId
    )

import Http
import Json.Decode as Decode
import List.Extra as List
import Time
import Utils.SimpleTime exposing (SimpleTime)


type alias EventList =
    List (Entry Event)


type Id a
    = Id String


stringFromId : Id a -> String
stringFromId (Id rawId) =
    rawId


type alias Entry a =
    ( Id a, a )


type alias Event =
    { name : String
    , teaser : String
    , description : String
    , occurrences : List (Entry Occurrence)
    }


type alias Occurrence =
    { start : Time.Posix
    , duration : Int
    , location : Entry Location
    }


type alias Location =
    { name : String
    , address : String
    }



-- Init


fetchEvents : (Result Http.Error EventList -> msg) -> Cmd msg
fetchEvents toMsg =
    Http.get
        { url = "/api/events"
        , expect = Http.expectJson toMsg decodeEventList
        }


findEvent : String -> EventList -> Maybe ( Id Event, Event )
findEvent rawId events =
    List.find (\( currentId, _ ) -> currentId == Id rawId) events



-- Decode


decodeEventList : Decode.Decoder EventList
decodeEventList =
    let
        mapMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
        mapMaybe map list =
            List.foldl
                (\item maybeResult ->
                    Maybe.andThen
                        (\result ->
                            case map item of
                                Just successor ->
                                    Just <| result ++ [ successor ]

                                Nothing ->
                                    Nothing
                        )
                        maybeResult
                )
                (Just [])
                list

        mapEvent : List (Entry Location) -> RawEvent -> Maybe (Entry Event)
        mapEvent locations rawEvent =
            let
                maybeOccurrences =
                    mapMaybe (mapOccurrence locations) rawEvent.occurrences
            in
            Maybe.map
                (\occurrences ->
                    ( Id rawEvent.id
                    , Event
                        rawEvent.name
                        rawEvent.teaser
                        rawEvent.description
                        occurrences
                    )
                )
                maybeOccurrences

        mapOccurrence : List (Entry Location) -> RawOccurrence -> Maybe (Entry Occurrence)
        mapOccurrence locations rawOccurrence =
            let
                maybeLocation =
                    List.find (\( Id id, _ ) -> id == rawOccurrence.locationId) locations
            in
            Maybe.map
                (\location ->
                    ( Id rawOccurrence.id
                    , Occurrence
                        rawOccurrence.start
                        rawOccurrence.duration
                        location
                    )
                )
                maybeLocation

        combine : List (Entry Location) -> List RawEvent -> Maybe EventList
        combine locations rawEvents =
            mapMaybe
                (mapEvent locations)
                rawEvents
    in
    Decode.map2
        combine
        (Decode.field "locations" (Decode.list decodeLocation))
        (Decode.field "events" (Decode.list decodeRawEvent))
        |> Decode.andThen
            (\maybeEvents ->
                case maybeEvents of
                    Just events ->
                        Decode.succeed events

                    Nothing ->
                        Decode.fail "An occurrence has used an invalid location id."
            )


type alias RawEvent =
    { id : String
    , name : String
    , teaser : String
    , description : String
    , occurrences : List RawOccurrence
    }


decodeRawEvent : Decode.Decoder RawEvent
decodeRawEvent =
    Decode.map5
        RawEvent
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "teaser" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "occurrences" (Decode.list decodeRawOccurrence))


type alias RawOccurrence =
    { id : String
    , start : Time.Posix
    , duration : Int
    , locationId : String
    }


decodeRawOccurrence : Decode.Decoder RawOccurrence
decodeRawOccurrence =
    Decode.map4
        RawOccurrence
        (Decode.field "id" Decode.string)
        (Decode.field "start" decodePosix)
        (Decode.field "duration" Decode.int)
        (Decode.field "location" Decode.string)


decodePosix : Decode.Decoder Time.Posix
decodePosix =
    Decode.int
        -- Timestamp in JSON is second-based
        |> Decode.map (\seconds -> seconds * 1000)
        |> Decode.map Time.millisToPosix


decodeLocation : Decode.Decoder (Entry Location)
decodeLocation =
    Decode.map3
        (\id name address ->
            ( Id id
            , { name = name
              , address = address
              }
            )
        )
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "address" Decode.string)
