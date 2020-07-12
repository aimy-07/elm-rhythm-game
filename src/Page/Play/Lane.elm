module Page.Play.Lane exposing
    ( Lane
    , allUnPress
    , isPressing
    , leftFromKey
    , new
    , press
    , pressingKeys
    , toKey
    , unPress
    , view
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, style)
import Page.Play.Key as Key exposing (Key)
import Utils exposing (classIf)


type Lane
    = Lane
        { key : Key
        , left : Int
        , isPressing : Bool
        }


new : Key -> Lane
new key =
    Lane
        { key = key
        , left = leftFromKey key
        , isPressing = False
        }


leftFromKey : Key -> Int
leftFromKey key =
    case key of
        Key.S ->
            50

        Key.D ->
            150

        Key.F ->
            250

        Key.J ->
            650

        Key.K ->
            750

        Key.L ->
            850


toKey : Lane -> Key
toKey (Lane { key }) =
    key


isPressing : Key -> Lane -> Bool
isPressing key (Lane lane) =
    if lane.key == key then
        lane.isPressing

    else
        False


pressingKeys : Key -> List Lane -> List Key
pressingKeys key lanes =
    lanes
        |> List.map
            (\lane ->
                if isPressing key lane then
                    Just key

                else
                    Nothing
            )
        |> List.filterMap identity


press : Key -> Lane -> Lane
press key (Lane lane) =
    if lane.key == key then
        Lane { lane | isPressing = True }

    else
        Lane lane


unPress : Key -> Lane -> Lane
unPress key (Lane lane) =
    if lane.key == key then
        Lane { lane | isPressing = False }

    else
        Lane lane


allUnPress : Lane -> Lane
allUnPress (Lane lane) =
    Lane { lane | isPressing = False }


view : Lane -> Html msg
view (Lane lane) =
    div
        [ class "playLane_lane"
        , classIf lane.isPressing "is-pressing"
        , style "left" (String.fromInt lane.left ++ "px")
        ]
        [ div [ class "playCenterLine_judgeArea" ] []
        , div [ class "playCenterLine_judgeAreaLine left" ] []
        , div [ class "playCenterLine_judgeAreaLine right" ] []
        , div [ class "playLane_keyText", classIf lane.isPressing "is-pressing" ] [ text <| Key.unwrap lane.key ]
        , div [ class "playJudgeEffect_effect", id <| "judgeEffect_" ++ Key.unwrap lane.key ] []
        , div [ class "playJudgeEffect_text", id <| "judgeEffectText_" ++ Key.unwrap lane.key ] []
        ]
