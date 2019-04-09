module Utils.NaiveDateTime exposing
    ( DateTime
    , build
    , day
    , decodeDateTime
    , encodeDateTime
    , hour
    , minute
    , month
    , monthNumeric
    , year
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, end, int, symbol)
import Time
import Utils.Format exposing (padInt)


type DateTime
    = DateTime Date Time


type Date
    = Date { year : Int, month : Time.Month, day : Int }


type Time
    = Time { hour : Int, minute : Int }



-- Create


build : { year : Int, month : Int, day : Int, hour : Int, minute : Int } -> Maybe DateTime
build v =
    let
        date =
            buildDate { year = v.year, month = v.month, day = v.day }

        time =
            buildTime { hour = v.hour, minute = v.minute }
    in
    Maybe.map2 DateTime date time


buildDate : { year : Int, month : Int, day : Int } -> Maybe Date
buildDate v =
    let
        yearResult =
            if v.year > 0 then
                Just v.year

            else
                Nothing

        monthResult =
            monthFromNumber v.month

        dayResult =
            Maybe.map2 Tuple.pair yearResult monthResult
                |> Maybe.andThen
                    (\( y, m ) ->
                        if v.day > 0 && v.day <= daysInMonth y m then
                            Just v.day

                        else
                            Nothing
                    )
    in
    Maybe.map3 (\y m d -> Date { year = y, month = m, day = d })
        yearResult
        monthResult
        dayResult


buildTime : { hour : Int, minute : Int } -> Maybe Time
buildTime v =
    let
        hourResult =
            if v.hour >= 0 && v.hour < 24 then
                Just v.hour

            else
                Nothing

        minuteResult =
            if v.minute >= 0 && v.minute < 60 then
                Just v.minute

            else
                Nothing
    in
    Maybe.map2 (\h m -> Time { hour = h, minute = m })
        hourResult
        minuteResult



-- Query


year : DateTime -> Int
year (DateTime (Date date) _) =
    date.year


month : DateTime -> Time.Month
month (DateTime (Date date) _) =
    date.month


monthNumeric : DateTime -> Int
monthNumeric dateTime =
    month dateTime
        |> numberFromMonth


day : DateTime -> Int
day (DateTime (Date date) _) =
    date.day


hour : DateTime -> Int
hour (DateTime _ (Time time)) =
    time.hour


minute : DateTime -> Int
minute (DateTime _ (Time time)) =
    time.minute



-- JSON


decodeDateTime : Decode.Decoder DateTime
decodeDateTime =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case Parser.run dateTimeParser raw of
                    Ok dateTime ->
                        Decode.succeed dateTime

                    Err error ->
                        Decode.fail (Parser.deadEndsToString error)
            )


encodeDateTime : DateTime -> Encode.Value
encodeDateTime dateTime =
    let
        y =
            String.fromInt <| year dateTime

        m =
            padInt <| monthNumeric dateTime

        d =
            padInt <| day dateTime

        h =
            padInt <| hour dateTime

        min =
            padInt <| minute dateTime
    in
    Encode.string
        (y ++ "-" ++ m ++ "-" ++ d ++ "T" ++ h ++ ":" ++ min)


dateTimeParser : Parser DateTime
dateTimeParser =
    Parser.succeed
        (\y m d h min ->
            { year = y, month = m, day = d, hour = h, minute = min }
        )
        |= int
        |. symbol "-"
        |= int
        |. symbol "-"
        |= int
        |. symbol "T"
        |= int
        |. symbol ":"
        |= int
        |. end
        |> Parser.andThen
            (\values ->
                build values
                    |> Maybe.map Parser.succeed
                    |> Maybe.withDefault (Parser.problem "The date or time is invalid.")
            )



-- Utils


monthFromNumber : Int -> Maybe Time.Month
monthFromNumber rawMonth =
    case rawMonth of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


numberFromMonth : Time.Month -> Int
numberFromMonth m =
    case m of
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



{-
   The following functions are taken from https://github.com/justinmimbs/date/blob/3.1.2/src/Date.elm,
   which is published under the following license. Namespacing was added where necessary.

   BSD 3-Clause

   Copyright (c) 2018, Justin Mimbs. All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this
      list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
   3. Neither the name of the copyright holder nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}


daysInMonth : Int -> Time.Month -> Int
daysInMonth y m =
    case m of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear y then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0
