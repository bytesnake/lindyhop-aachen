module Utils.TimeFormat exposing (date, fullDate, time)

import Time
import Utils.Format exposing (padInt)


fullDate : Time.Zone -> Time.Posix -> String
fullDate zone posix =
    let
        formattedDate =
            date zone posix

        formattedTime =
            time zone posix
    in
    formattedDate ++ " " ++ formattedTime


date : Time.Zone -> Time.Posix -> String
date zone posix =
    let
        day =
            Time.toDay zone posix
                |> padInt

        month =
            Time.toMonth zone posix
                |> padInt

        year =
            Time.toYear zone posix
                |> String.fromInt
    in
    day ++ "." ++ month ++ "." ++ year


time : Time.Zone -> Time.Posix -> String
time zone posix =
    let
        hour =
            Time.toHour zone posix
                |> padInt

        minute =
            Time.toMinute zone posix
                |> padInt
    in
    hour ++ ":" ++ minute
