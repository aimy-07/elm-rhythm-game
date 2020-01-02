module Page.Init exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Page


view : Html msg
view =
    div [ class "title_back" ] [ Page.viewLoading ]
