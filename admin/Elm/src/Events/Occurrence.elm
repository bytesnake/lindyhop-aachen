module Events.Occurrence exposing
    ( Occurrence
    , duration
    , location
    , occurrenceDecoder
    , start
    )

import Events.Location as Location exposing (Location, locationDecoder)
import Json.Decode as Decode
import Json.Encode as Encode
import Time


type Occurrence
    = Occurrence
        { start : Time.Posix
        , duration : Duration
        , location : Location
        }


start : Occurrence -> Time.Posix
start (Occurrence occurrence) =
    occurrence.start


duration : Occurrence -> Duration
duration (Occurrence occurrence) =
    occurrence.duration


location : Occurrence -> Location
location (Occurrence occurrence) =
    occurrence.location


occurrenceDecoder : Decode.Decoder Occurrence
occurrenceDecoder =
    Decode.map3
        (\start_ duration_ location_ ->
            Occurrence
                { start = start_
                , duration = duration_
                , location = location_
                }
        )
        (Decode.field "start" posixDecoder)
        (Decode.field "duration" durationDecoder)
        (Decode.field "location" locationDecoder)


posixDecoder : Decode.Decoder Time.Posix
posixDecoder =
    Decode.int
        -- Time.millisToPosix expects milliseconds
        |> Decode.map (\seconds -> seconds * 1000)
        |> Decode.map Time.millisToPosix


type Duration
    = Seconds Int


durationDecoder : Decode.Decoder Duration
durationDecoder =
    Decode.map
        Seconds
        Decode.int
