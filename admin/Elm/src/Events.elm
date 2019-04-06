module Events exposing
    ( Events, Id, stringFromId, Event, Occurrence, Location
    , fetchEvents
    , map, locations, findEvent
    )

{-| Fetches, stores, and makes accessible the events from the backend.


# Types

@docs Events, Id, stringFromId, Entry, Event, Occurrence, Location


# Fetch

@docs fetchEvents


# Access

@docs map, locations, findEvent

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import List.Extra as List
import Time
import Utils.SimpleTime exposing (SimpleTime)


{-| Wrapper for ids to prevent mixing of ids from different types at compile time.
-}
type Id a
    = Id String


idFromString : String -> Id a
idFromString rawId =
    Id rawId


{-| Convert an id to a string for use in URLs.
-}
stringFromId : Id a -> String
stringFromId (Id rawId) =
    rawId


{-| Associates an id with an item.
-}
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



-- Events


type alias RefEvent =
    { name : String
    , teaser : String
    , description : String
    , occurrences : List ( Id Occurrence, RefOccurrence )
    }


type alias RefOccurrence =
    { start : Time.Posix
    , duration : Int
    , locationId : Id Location
    }


{-| Event container.
-}
type Events
    = Events (Dict String Location) (List ( Id Event, RefEvent ))


{-| Extracts the locations.
-}
locations : Events -> List (Entry Location)
locations (Events locs _) =
    Dict.toList locs
        |> List.map (Tuple.mapFirst Id)


{-| Maps over all entries.
-}
map : (Entry Event -> b) -> Events -> List b
map mapping (Events locs events) =
    List.map
        (\( id, refEvent ) ->
            let
                locationEntry locId =
                    Dict.get (stringFromId locId) locs
                        |> Maybe.map (\loc -> ( locId, loc ))
                        -- This case will never happen.
                        |> Maybe.withDefault ( Id "notFound", { name = "NotFound", address = "NoWhere" } )

                mapOccurrence ( occId, refOccurrence ) =
                    ( occId
                    , { start = refOccurrence.start
                      , duration = refOccurrence.duration
                      , location = locationEntry refOccurrence.locationId
                      }
                    )

                occurrences =
                    List.map
                        mapOccurrence
                        refEvent.occurrences
            in
            ( id
            , { name = refEvent.name
              , teaser = refEvent.teaser
              , description = refEvent.description
              , occurrences = occurrences
              }
            )
        )
        events
        |> List.map mapping



-- Init


{-| The HTTP command to fetch the events.
-}
fetchEvents : (Result Http.Error Events -> msg) -> Cmd msg
fetchEvents toMsg =
    Http.get
        { url = "/api/events"
        , expect = Http.expectJson toMsg decodeEventList
        }


{-| Extracts a single event by its id.
-}
findEvent : String -> Events -> Maybe ( Id Event, Event )
findEvent rawId events =
    let
        eventList =
            map identity events
    in
    List.find (\( currentId, _ ) -> currentId == Id rawId) eventList



-- Decode


decodeEventList : Decode.Decoder Events
decodeEventList =
    let
        mapEvent : RawEvent -> ( Id Event, RefEvent )
        mapEvent rawEvent =
            let
                occurrences =
                    List.map mapOccurrence rawEvent.occurrences
            in
            ( Id rawEvent.id
            , RefEvent
                rawEvent.name
                rawEvent.teaser
                rawEvent.description
                occurrences
            )

        mapOccurrence : RawOccurrence -> ( Id Occurrence, RefOccurrence )
        mapOccurrence rawOccurrence =
            ( Id rawOccurrence.id
            , RefOccurrence
                rawOccurrence.start
                rawOccurrence.duration
                (Id rawOccurrence.locationId)
            )
    in
    Decode.map2
        (\locs events ->
            let
                locsDict =
                    List.map (Tuple.mapFirst stringFromId) locs
                        |> Dict.fromList
            in
            Events locsDict (List.map mapEvent events)
        )
        (Decode.field "locations" (Decode.list decodeLocation))
        (Decode.field "events" (Decode.list decodeRawEvent))


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
