module Page exposing (Page(..), view, viewLoaded, viewLoading)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)


type Page
    = Other
    | Home
    | Login
    | Play
    | Error


view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title
    , body = [ content ]
    }


viewLoading : Html msg
viewLoading =
    div [ class "loading_container" ]
        [ div
            [ class "loading_boxContainer" ]
            [ div [ class "loading_box is-first" ] []
            , div [ class "loading_box is-second" ] []
            , div [ class "loading_box is-third" ] []
            , div [ class "loading_box is-fourth" ] []
            ]
        ]


viewLoaded : Html msg
viewLoaded =
    div [ class "loading_container is-loaded" ]
        [ div
            [ class "loading_boxContainer" ]
            [ div [ class "loading_box is-loaded" ] [] ]
        ]
