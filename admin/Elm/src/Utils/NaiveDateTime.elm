module Utils.NaiveDateTime exposing (Date(..), DateTime(..))

import Date
import Json.Decode as Decode
import Time


type DateTime
    = DateTime Date Time


type Date
    = Date { year : Int, month : Time.Month, day : Int }


type Time
    = Time { hour : Int, minute : Int }
