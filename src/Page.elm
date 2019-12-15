module Page exposing (Page(..), cmdIf, view, viewIf, viewLoaded, viewLoading)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)


type Page
    = Other
    | Home
    | Login
    | Play


view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title
    , body = [ content ]
    }


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
