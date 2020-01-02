module Page.Blank exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


view : { title : String, content : Html msg }
view =
    { title = "Title"
    , content = div [ class "mainWide" ] [ viewContents ]
    }


viewContents : Html msg
viewContents =
    div [ class "init_back" ] []
