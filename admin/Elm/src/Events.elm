module Events exposing
    ( Events, Id, stringFromId, Event, Occurrence, Location
    , fetchEvents
    , map, locations, findEvent, findLocation
    , encodeEvent
    )

{-| Fetches, stores, and makes accessible the events from the backend.


# Types

@docs Events, Id, stringFromId, Entry, Event, Occurrence, Location


# Fetch

@docs fetchEvents


# Access

@docs map, locations, findEvent, findLocation

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Utils.NaiveDateTime as Naive exposing (DateTime)


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
    , occurrences : List Occurrence
    }


type alias Occurrence =
    { start : DateTime
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
    , occurrences : List RefOccurrence
    }


type alias RefOccurrence =
    { start : DateTime
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

                mapOccurrence refOccurrence =
                    { start = refOccurrence.start
                    , duration = refOccurrence.duration
                    , location = locationEntry refOccurrence.locationId
                    }

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
        , expect = Http.expectJson toMsg decodeEvents
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


{-| Extracts a single location by its id.
-}
findLocation : String -> Events -> Maybe ( Id Location, Location )
findLocation rawId (Events locs _) =
    Dict.toList locs
        |> List.find (\( currentId, _ ) -> currentId == rawId)
        |> Maybe.map (Tuple.mapFirst Id)


updateEvent : Id Event -> Event -> (Result Http.Error () -> msg) -> Cmd msg
updateEvent id event toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/event/" ++ stringFromId id
        , body = Http.jsonBody (encodeEvent event)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- Decode


decodeEvents : Decode.Decoder Events
decodeEvents =
    let
        mapEvent : RawEvent -> ( Id Event, RefEvent )
        mapEvent rawEvent =
            ( Id rawEvent.id
            , RefEvent
                rawEvent.name
                rawEvent.teaser
                rawEvent.description
                rawEvent.occurrences
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
    , occurrences : List RefOccurrence
    }


decodeRawEvent : Decode.Decoder RawEvent
decodeRawEvent =
    Decode.map5
        RawEvent
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "teaser" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "occurrences" (Decode.list decodeOccurrence))


decodeOccurrence : Decode.Decoder RefOccurrence
decodeOccurrence =
    Decode.map3
        RefOccurrence
        (Decode.field "start" Naive.decodeDateTime)
        (Decode.field "duration" Decode.int)
        (Decode.field "location" decodeId)


decodeId : Decode.Decoder (Id a)
decodeId =
    Decode.string |> Decode.map Id


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



-- Encode


encodeEvent : Event -> Encode.Value
encodeEvent event =
    Encode.object
        [ ( "name", Encode.string event.name )
        , ( "teaser", Encode.string event.teaser )
        , ( "description", Encode.string event.description )
        , ( "occurrences", Encode.list encodeOccurrence event.occurrences )
        ]


encodeOccurrence : Occurrence -> Encode.Value
encodeOccurrence occurrence =
    let
        locationId =
            Tuple.first occurrence.location
    in
    Encode.object
        [ ( "start", Naive.encodeDateTime occurrence.start )
        , ( "duration", Encode.int occurrence.duration )
        , ( "location", encodeId locationId )
        ]


encodeId : Id a -> Encode.Value
encodeId (Id id) =
    Encode.string id
