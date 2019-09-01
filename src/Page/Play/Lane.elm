module Page.Play.Lane exposing
    ( Lane
    , leftFromKeyStr
    , new
    , toIsPresing
    , toKeyStr
    , updateKeyDown
    , updateKeyUp
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.KeyStr as KeyStr exposing (KeyStr)


type Lane
    = Lane
        { isPressing : Bool
        , keyStr : KeyStr
        , left : Int
        }


new : KeyStr -> Lane
new keyStr =
    Lane
        { isPressing = False
        , keyStr = keyStr
        , left = leftFromKeyStr keyStr
        }


leftFromKeyStr : KeyStr -> Int
leftFromKeyStr keyStr =
    case keyStr of
        "S" ->
            50

        "D" ->
            150

        "F" ->
            250

        "J" ->
            650

        "K" ->
            750

        "L" ->
            850

        _ ->
            -1000


toKeyStr : Lane -> KeyStr
toKeyStr (Lane { keyStr }) =
    keyStr


toIsPresing : Lane -> Bool
toIsPresing (Lane { isPressing }) =
    isPressing


updateKeyDown : Lane -> Lane
updateKeyDown (Lane lane) =
    Lane { lane | isPressing = True }


updateKeyUp : Lane -> Lane
updateKeyUp (Lane lane) =
    Lane { lane | isPressing = False }


view : Lane -> Html msg
view (Lane { isPressing, keyStr, left }) =
    let
        isPressingStyleClass =
            if isPressing then
                " is-pressing"

            else
                ""
    in
    div
        [ class <| "play_lane" ++ isPressingStyleClass
        , style "left" (String.fromInt left ++ "px")
        ]
        [ div
            [ class <| "lane_text" ++ isPressingStyleClass ]
            [ text keyStr ]
        ]
