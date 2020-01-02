module Page.Init exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Page


view : { title : String, content : Html msg }
view =
    { title = "Title"
    , content = div [ class "mainWide" ] [ viewContents ]
    }


viewContents : Html msg
viewContents =
    div [ class "init_back" ] [ Page.viewLoading ]
