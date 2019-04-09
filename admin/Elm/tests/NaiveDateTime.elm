module NaiveDateTime exposing (suite)

import Date
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Test exposing (..)
import Utils.NaiveDateTime as Naive


suite : Test
suite =
    describe "DateTime"
        [ fuzz2 (Fuzz.tuple3 ( intRange 1900 2100, intRange 1 12, intRange 1 31 )) (Fuzz.tuple ( intRange 0 23, intRange 0 59 )) "date creation is consistent with other library" <|
            \( y, m, d ) ( h, min ) ->
                case Naive.build { year = y, month = m, day = d, hour = h, minute = min } of
                    Just dateTime ->
                        let
                            referenceDate =
                                Date.fromCalendarDate y (Date.numberToMonth m) d

                            actual =
                                { year = Naive.year dateTime
                                , month = Naive.month dateTime
                                , day = Naive.day dateTime
                                }

                            expected =
                                { year = Date.year referenceDate
                                , month = Date.month referenceDate
                                , day = Date.day referenceDate
                                }
                        in
                        Expect.equal expected actual

                    Nothing ->
                        let
                            referenceDate =
                                Date.fromCalendarDate y (Date.numberToMonth m) d
                        in
                        Expect.notEqual (Date.day referenceDate) d
        , todo "test encoding and decoding"
        ]
