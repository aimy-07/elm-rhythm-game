module Page.Play.LinePosition exposing (LinePosition, allLines, isSamePosition, new, styleLeft, unwrap, viewLine)

import Html exposing (..)
import Html.Attributes exposing (..)


type LinePosition
    = S
    | D
    | F
    | J
    | K
    | L
    | Invalid


new : String -> LinePosition
new keyStr =
    case keyStr of
        "S" ->
            S

        "D" ->
            D

        "F" ->
            F

        "J" ->
            J

        "K" ->
            K

        "L" ->
            L

        _ ->
            Invalid


unwrap : LinePosition -> String
unwrap linePosition =
    case linePosition of
        S ->
            "S"

        D ->
            "D"

        F ->
            "F"

        J ->
            "J"

        K ->
            "K"

        L ->
            "L"

        Invalid ->
            Debug.todo "" "無効なLinePositionです"


allLines : List LinePosition
allLines =
    [ S, D, F, J, K, L ]


isSamePosition : LinePosition -> LinePosition -> Bool
isSamePosition a b =
    a == b


styleLeft : LinePosition -> String
styleLeft linePosition =
    case linePosition of
        S ->
            String.fromInt 50 ++ "px"

        D ->
            String.fromInt 150 ++ "px"

        F ->
            String.fromInt 250 ++ "px"

        J ->
            String.fromInt 650 ++ "px"

        K ->
            String.fromInt 750 ++ "px"

        L ->
            String.fromInt 850 ++ "px"

        Invalid ->
            "-1000px"


viewLine : LinePosition -> List LinePosition -> Html msg
viewLine linePosition pressingLines =
    let
        isPressing =
            List.member linePosition pressingLines

        isPressingStyleClass =
            if isPressing then
                " is-pressing"

            else
                ""
    in
    div
        [ class <| "play_line" ++ isPressingStyleClass
        , style "left" (styleLeft linePosition)
        ]
        [ div
            [ class <| "line_text" ++ isPressingStyleClass
            ]
            [ text <| unwrap linePosition ]
        ]
