module Utils.TimeFormat exposing (stringFromDate, stringFromPosix, stringFromSimpleTime)

import Date exposing (Date)
import Time
import Utils.SimpleTime as SimpleTime exposing (SimpleTime)


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
