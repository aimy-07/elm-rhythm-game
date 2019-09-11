module Page exposing (Page(..), cmdIf, updateIf, view, viewIf, viewLoading)

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


viewLoading : Html msg
viewLoading =
    div [ class "sk-fading-circle" ]
        [ div [ class "sk-circle1 sk-circle" ] []
        , div [ class "sk-circle2 sk-circle" ] []
        , div [ class "sk-circle3 sk-circle" ] []
        , div [ class "sk-circle4 sk-circle" ] []
        , div [ class "sk-circle5 sk-circle" ] []
        , div [ class "sk-circle6 sk-circle" ] []
        , div [ class "sk-circle7 sk-circle" ] []
        , div [ class "sk-circle8 sk-circle" ] []
        , div [ class "sk-circle9 sk-circle" ] []
        , div [ class "sk-circle10 sk-circle" ] []
        , div [ class "sk-circle11 sk-circle" ] []
        , div [ class "sk-circle12 sk-circle" ] []
        ]
