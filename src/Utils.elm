module Utils exposing (classIf, cmdIf, subIf, viewIf)

import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)


viewIf : Bool -> Html msg -> Html msg
viewIf show element =
    if show then
        element

    else
        text ""


cmdIf : Bool -> Cmd msg -> Cmd msg
cmdIf execute cmd =
    if execute then
        cmd

    else
        Cmd.none


subIf : Bool -> Sub msg -> Sub msg
subIf execute sub =
    if execute then
        sub

    else
        Sub.none


classIf : Bool -> String -> Attribute msg
classIf condition clsStr =
    if condition then
        class clsStr

    else
        class ""
