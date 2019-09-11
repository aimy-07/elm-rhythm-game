module Page.Play.Lanes exposing
    ( Lanes
    , init
    , isPressing
    , updateKeyDown
    , updateKeyUp
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.KeyStr as KeyStr exposing (KeyStr)
import Page.Play.Lane as Lane exposing (Lane)


type Lanes
    = Lanes (List Lane)


init : Lanes
init =
    Lanes
        (List.map (\keyStr -> Lane.new keyStr) KeyStr.allKeyStr)


isPressing : KeyStr -> Lanes -> Bool
isPressing keyStr (Lanes lanes) =
    lanes
        |> List.filter (\lane -> Lane.toKeyStr lane == keyStr)
        |> List.head
        |> Maybe.map (\lane -> Lane.toIsPresing lane)
        |> Maybe.withDefault False


updateKeyDown : KeyStr -> Lanes -> Lanes
updateKeyDown keyStr (Lanes lanes) =
    Lanes
        (lanes
            |> List.map
                (\lane ->
                    if Lane.toKeyStr lane == keyStr then
                        Lane.updateKeyDown lane

                    else
                        lane
                )
        )


updateKeyUp : KeyStr -> Lanes -> Lanes
updateKeyUp keyStr (Lanes lanes) =
    Lanes
        (lanes
            |> List.map
                (\lane ->
                    if Lane.toKeyStr lane == keyStr then
                        Lane.updateKeyUp lane

                    else
                        lane
                )
        )


view : Lanes -> Html msg
view (Lanes lanes) =
    div []
        [ div [ class "playCenterLine_judgeAreaLine outer center" ] []
        , div [ class "playCenterLine_judgeAreaLine outer left" ] []
        , div [ class "playCenterLine_judgeAreaLine outer right" ] []
        , div []
            (List.map (\lane -> Lane.view lane) lanes)
        ]
