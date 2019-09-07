module Page exposing (Page(..), cmdIf, updateIf, view, viewIf)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Page
    = Other
    | Home
    | Play


view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title
    , body = [ content ]
    }


viewIf : Bool -> Html msg -> Html msg
viewIf show element =
    case show of
        True ->
            element

        False ->
            text ""


cmdIf : Bool -> Cmd msg -> Cmd msg
cmdIf execute cmd =
    case execute of
        True ->
            cmd

        False ->
            Cmd.none


updateIf : Bool -> (a -> a) -> a -> a
updateIf update func variable =
    case update of
        True ->
            func variable

        False ->
            variable
