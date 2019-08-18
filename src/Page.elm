module Page exposing (Page(..), view, viewIf)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route)


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
