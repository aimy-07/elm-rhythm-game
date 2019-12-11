module Page.Play.Lane exposing
    ( Lane
    , allUnPress
    , isPressing
    , leftFromKeyStr
    , new
    , press
    , toKeyStr
    , unPress
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.KeyStr exposing (KeyStr)


type Lane
    = Lane
        { keyStr : KeyStr
        , left : Int
        , isPressing : Bool
        }


new : KeyStr -> Lane
new keyStr =
    Lane
        { keyStr = keyStr
        , left = leftFromKeyStr keyStr
        , isPressing = False
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


isPressing : KeyStr -> Lane -> Bool
isPressing keyStr (Lane lane) =
    if lane.keyStr == keyStr then
        lane.isPressing

    else
        False


press : KeyStr -> Lane -> Lane
press keyStr (Lane lane) =
    if lane.keyStr == keyStr then
        Lane { lane | isPressing = True }

    else
        Lane lane


unPress : KeyStr -> Lane -> Lane
unPress keyStr (Lane lane) =
    if lane.keyStr == keyStr then
        Lane { lane | isPressing = False }

    else
        Lane lane


allUnPress : Lane -> Lane
allUnPress (Lane lane) =
    Lane { lane | isPressing = False }


view : Lane -> Html msg
view (Lane lane) =
    div
        [ classList
            [ ( "playLane_lane", True )
            , ( "is-pressing", lane.isPressing )
            ]
        , style "left" (String.fromInt lane.left ++ "px")
        ]
        [ div [ class "playCenterLine_judgeArea" ] []
        , div [ class "playCenterLine_judgeAreaLine left" ] []
        , div [ class "playCenterLine_judgeAreaLine right" ] []
        , div
            [ classList
                [ ( "playLane_keyText", True )
                , ( "is-pressing", lane.isPressing )
                ]
            ]
            [ text lane.keyStr ]
        , div
            [ class "playJudgeEffect_effect"
            , id <| "judgeEffect_" ++ lane.keyStr
            ]
            []
        , div
            [ class "playJudgeEffect_text"
            , id <| "judgeEffectText_" ++ lane.keyStr
            ]
            []
        ]
