module Utils.TimeFormat exposing (date, fullDate, time)

import Utils.Format exposing (padInt)
import Utils.NaiveDateTime as Naive


fullDate : Naive.DateTime -> String
fullDate dateTime =
    let
        formattedDate =
            date dateTime

        formattedTime =
            time dateTime
    in
    formattedDate ++ " " ++ formattedTime


date : Naive.DateTime -> String
date dateTime =
    let
        day =
            Naive.day dateTime
                |> padInt

        month =
            Naive.monthNumeric dateTime
                |> padInt

        year =
            Naive.year dateTime
                |> String.fromInt
    in
    day ++ "." ++ month ++ "." ++ year


time : Naive.DateTime -> String
time dateTime =
    let
        hour =
            Naive.hour dateTime
                |> padInt

        minute =
            Naive.minute dateTime
                |> padInt
    in
    hour ++ ":" ++ minute
