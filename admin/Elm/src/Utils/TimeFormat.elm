module Utils.TimeFormat exposing (date, fullDate, time)

import Time


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
                |> numericFromMonth
                |> padInt

        year =
            Time.toYear zone posix
                |> String.fromInt
    in
    day ++ "." ++ month ++ "." ++ year


numericFromMonth : Time.Month -> Int
numericFromMonth month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


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


padInt : Int -> String
padInt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n
