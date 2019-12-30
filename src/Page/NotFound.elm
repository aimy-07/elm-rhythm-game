module Page.NotFound exposing (view)

import Html exposing (Html, h1, text)



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content = h1 [] [ text "Not Found" ]
    }
