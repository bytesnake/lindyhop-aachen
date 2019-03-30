module Events.Event exposing
    ( Event
    , FullEvent
    , description
    , event
    , fullEventDecoder
    , name
    , occurrences
    , teaser
    )

import Events.Occurrence exposing (Occurrence, occurrenceDecoder)
import Json.Decode as Decode
import Json.Encode as Encode


type Event
    = Event
        { name : String
        , teaser : String
        , description : String
        }


name : Event -> String
name (Event event_) =
    event_.name


teaser : Event -> String
teaser (Event event_) =
    event_.teaser


description : Event -> String
description (Event event_) =
    event_.description


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.map3
        (\name_ teaser_ description_ ->
            Event
                { name = name_
                , teaser = teaser_
                , description = description_
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "teaser" Decode.string)
        (Decode.field "description" Decode.string)


type FullEvent
    = FullEvent
        { event : Event
        , occurrences : List Occurrence
        }


event : FullEvent -> Event
event (FullEvent fullEvent) =
    fullEvent.event


occurrences : FullEvent -> List Occurrence
occurrences (FullEvent fullEvent) =
    fullEvent.occurrences


fullEventDecoder : Decode.Decoder FullEvent
fullEventDecoder =
    Decode.map2
        (\event_ occurrences_ ->
            FullEvent
                { event = event_
                , occurrences = occurrences_
                }
        )
        eventDecoder
        (Decode.field "occurrences" (Decode.list occurrenceDecoder))
