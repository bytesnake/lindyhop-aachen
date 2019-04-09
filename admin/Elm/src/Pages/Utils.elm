module Pages.Utils exposing (fields, labeled, viewDateTimeInput, viewInputNumber, viewInputText, viewTextArea)

import Css exposing (center, column, em, flexStart, row, zero)
import Css.Global as Css
import Html.Styled as Html exposing (Html, div, input, label, text, textarea)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onInput)
import Utils.NaiveDateTime as Naive
import Utils.TimeFormat as TimeFormat


viewInputText : String -> String -> (String -> msg) -> Html msg
viewInputText lbl val inputMsg =
    labeled lbl
        [ input [ type_ "text", value val, onInput inputMsg ] []
        ]


viewInputNumber : String -> Int -> (String -> msg) -> Html msg
viewInputNumber lbl val inputMsg =
    labeled lbl
        [ input [ type_ "number", value <| String.fromInt val, onInput inputMsg ] []
        ]


viewTextArea : String -> String -> (String -> msg) -> Html msg
viewTextArea lbl val inputMsg =
    labeled lbl
        [ textarea [ value val, onInput inputMsg ] []
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
    labeled lbl
        [ input [ type_ "date", value date, onInput toMsgs.dateChanged ] []
        , input [ type_ "time", value time, onInput toMsgs.timeChanged ] []
        ]


labeled : String -> List (Html msg) -> Html msg
labeled lbl content =
    label [ css [ labelStyle ] ] (text lbl :: content)


labelStyle : Css.Style
labelStyle =
    Css.batch
        [ Css.displayFlex
        , Css.flexDirection column
        , Css.alignItems flexStart
        ]


fields : List (Html msg) -> Html msg
fields content =
    div [ css [ Css.children [ Css.everything [ inputSpacingStyle ] ] ] ]
        content


inputSpacingStyle : Css.Style
inputSpacingStyle =
    Css.batch
        [ Css.marginTop (em 1)
        , Css.marginBottom (em 1)
        ]
