module Pages.Utils exposing
    ( breadcrumbs
    , fields
    , labeled
    , viewDateTimeInput
    , viewInputNumber
    , viewInputText
    , viewTextArea
    )

import Css exposing (center, column, em, flexStart, none, row, zero)
import Css.Global as Css
import Html.Styled as Html exposing (Html, a, div, input, label, li, nav, ol, text, textarea)
import Html.Styled.Attributes exposing (css, href, type_, value)
import Html.Styled.Events exposing (onInput)
import Routes exposing (Route)
import Utils.NaiveDateTime as Naive
import Utils.TimeFormat as TimeFormat



-- Navigation


breadcrumbs : List Route -> Route -> Html msg
breadcrumbs routes current =
    let
        entriesHtml =
            List.map
                (\route ->
                    a [ href (Routes.toRelativeUrl <| route) ] [ text <| Routes.routeName route ]
                )
                routes
                ++ [ text <| Routes.routeName current ]

        breadcrumbStyle =
            Css.batch
                [ Css.listStyleType none
                , Css.padding zero
                , Css.displayFlex
                , Css.flexDirection row
                , Css.children
                    [ Css.typeSelector "li"
                        [ Css.adjacentSiblings
                            [ Css.typeSelector "li"
                                [ Css.marginLeft (em 0.5)
                                , Css.before
                                    [ Css.property "content" "\">\""
                                    , Css.marginRight (em 0.5)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
    in
    nav []
        [ ol [ css [ breadcrumbStyle ] ]
            (List.map
                (\entryHtml ->
                    li [] [ entryHtml ]
                )
                entriesHtml
            )
        ]



-- Forms


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
