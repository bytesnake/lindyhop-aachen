module Events exposing
    ( Event, Occurrence, Location, Store, Events, Locations
    , fetchEvents, updateEvent, updateLocation
    , locations, events, mapEvents, mapLocations
    )

{-| Fetches, stores, and makes accessible the events from the backend.


# Types

@docs Event, Occurrence, Location, Store, Events, Locations


# API

@docs fetchEvents, updateEvent, updateLocation


# Access

@docs locations, events, mapEvents, mapLocations

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import Http
import IdDict exposing (Id, IdDict)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Url.Builder
import Utils.NaiveDateTime as Naive exposing (DateTime)


type alias Event =
    { name : String
    , teaser : String
    , description : String
    , occurrences : List Occurrence
    }


type alias Occurrence =
    { start : DateTime
    , duration : Int
    , locationId : Id Location
    }


type alias Location =
    { name : String
    , address : String
    }



-- Store


{-| Event container.
-}
type Store
    = Store Locations Events


type alias Locations =
    IdDict Location


type alias Events =
    IdDict Event


{-| Extracts the locations.
-}
locations : Store -> IdDict Location
locations (Store locs _) =
    locs


events : Store -> IdDict Event
events (Store _ evts) =
    evts


mapLocations : (Id Location -> Location -> b) -> Store -> List b
mapLocations mapping store =
    locations store
        |> IdDict.map mapping


mapEvents : (Id Event -> Event -> b) -> Store -> List b
mapEvents mapping store =
    events store
        |> IdDict.map mapping



-- Init


apiUrl : List String -> String
apiUrl path =
    let
        base =
            [ "api" ]
    in
    Url.Builder.absolute (base ++ path) []


{-| The HTTP command to fetch the events.
-}
fetchEvents : (Result Http.Error Store -> msg) -> Cmd msg
fetchEvents toMsg =
    Http.get
        { url = apiUrl [ "events" ]
        , expect = Http.expectJson toMsg decodeEvents
        }


updateEvent : Id Event -> Event -> (Result Http.Error () -> msg) -> Cmd msg
updateEvent id event toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl [ "events", IdDict.encodeIdForUrl id ]
        , body = Http.jsonBody (encodeEvent event)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


updateLocation : Id Location -> Location -> (Result Http.Error () -> msg) -> Cmd msg
updateLocation id location toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ["locations", IdDict.encodeIdForUrl id]
        , body = Http.jsonBody (encodeLocation location)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- Decode


decodeEvents : Decode.Decoder Store
decodeEvents =
    let
        defaultLocation =
            Location "" ""

        defaultEvent =
            Event "" "" "" []
    in
    Decode.field "locations" (IdDict.decodeIdDict defaultLocation decodeLocation)
        |> Decode.andThen
            (\locs ->
                Decode.map
                    (Store locs)
                    (Decode.field "events" (IdDict.decodeIdDict defaultEvent (decodeEvent locs)))
            )


decodeEvent : IdDict Location -> Decode.Decoder Event
decodeEvent locs =
    Decode.map4
        Event
        (Decode.field "name" Decode.string)
        (Decode.field "teaser" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "occurrences" (Decode.list (decodeOccurrence locs)))


decodeOccurrence : IdDict Location -> Decode.Decoder Occurrence
decodeOccurrence locs =
    Decode.field "location_id" Decode.string
        |> Decode.andThen
            (\rawId ->
                case IdDict.validate rawId locs of
                    Just id ->
                        Decode.succeed id

                    Nothing ->
                        Decode.fail "Invalid location id."
            )
        |> Decode.map3
            Occurrence
            (Decode.field "start" Naive.decodeDateTime)
            (Decode.field "duration" Decode.int)


decodeLocation : Decode.Decoder Location
decodeLocation =
    Decode.map2
        Location
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
    Encode.object
        [ ( "start", Naive.encodeDateTime occurrence.start )
        , ( "duration", Encode.int occurrence.duration )
        , ( "location_id", IdDict.encodeId occurrence.locationId )
        ]


encodeLocation : Location -> Encode.Value
encodeLocation location =
    Encode.object
        [ ( "name", Encode.string location.name )
        , ( "address", Encode.string location.address )
        ]
