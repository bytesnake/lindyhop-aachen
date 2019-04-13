module Pages.Utils exposing (viewDateTimeInput, viewInputNumber, viewInputText, viewTextArea)

import Html exposing (Html, input, label, text, textarea)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Utils.NaiveDateTime as Naive
import Utils.TimeFormat as TimeFormat


viewInputText : String -> String -> (String -> msg) -> Html msg
viewInputText lbl val inputMsg =
    label []
        [ text lbl
        , input [ type_ "text", value val, onInput inputMsg ] []
        ]


viewInputNumber : String -> Int -> (String -> msg) -> Html msg
viewInputNumber lbl val inputMsg =
    label []
        [ text lbl
        , input [ type_ "number", value <| String.fromInt val, onInput inputMsg ] []
        ]


viewTextArea : String -> String -> (String -> msg) -> Html msg
viewTextArea lbl val inputMsg =
    label []
        [ text lbl
        , textarea [ value val, onInput inputMsg ] []
        ]


viewDateTimeInput :
    String
    -> Naive.DateTime
    -> { dateChanged : String -> msg, timeChanged : String -> msg }
    -> Html msg
viewDateTimeInput lbl val toMsgs =
    let
        date =
            TimeFormat.dateIso val

        time =
            TimeFormat.time val
    in
    label []
        [ text lbl
        , input [ type_ "date", value date, onInput toMsgs.dateChanged ] []
        , input [ type_ "time", value time, onInput toMsgs.timeChanged ] []
        ]
