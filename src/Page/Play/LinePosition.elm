module Page.Play.LinePosition exposing (LinePosition, allLines, isSamePosition, new, styleLeft, unwrap)

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
            String.fromInt 0 ++ "px"

        D ->
            String.fromInt 100 ++ "px"

        F ->
            String.fromInt 200 ++ "px"

        J ->
            String.fromInt 500 ++ "px"

        K ->
            String.fromInt 600 ++ "px"

        L ->
            String.fromInt 700 ++ "px"

        Invalid ->
            "-1000px"
