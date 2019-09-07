module Page.Home.AllMusicInfoList exposing (AllMusicInfoList, create, init, isLoaded, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MusicInfo as MusicInfo exposing (MusicInfo)
import MusicInfo.Mode as Mode exposing (Mode)
import Route


type AllMusicInfoList
    = NotLoaded
    | Loaded (List MusicInfo)


init : AllMusicInfoList
init =
    NotLoaded


isLoaded : AllMusicInfoList -> Bool
isLoaded allMusicInfoList =
    case allMusicInfoList of
        Loaded _ ->
            True

        NotLoaded ->
            False


create : List MusicInfo -> AllMusicInfoList
create musicInfos =
    Loaded musicInfos


view : AllMusicInfoList -> Html msg
view allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfos ->
            div [] (List.map viewMusicInfo musicInfos)

        NotLoaded ->
            text ""


viewMusicInfo : MusicInfo -> Html msg
viewMusicInfo musicInfo =
    -- TODO: modeによって背景色を変える
    div []
        [ div []
            [ a
                [ Route.href <| Route.Play <| MusicInfo.toCsvFileName musicInfo ]
                [ text <| MusicInfo.toMusicName musicInfo ]
            ]
        , div [] [ text <| MusicInfo.toComposer musicInfo ]
        , div []
            [ text <| "楽曲Lv: " ++ String.fromInt (MusicInfo.toLevel musicInfo)
            , text <| "\u{3000}Bpm: " ++ String.fromInt (MusicInfo.toBpm musicInfo)
            , text <| "\u{3000}曲の長さ: " ++ MusicInfo.toStringTime (MusicInfo.toFullTime musicInfo)
            , text <| "\u{3000}最大Combo: " ++ String.fromInt (MusicInfo.toMaxCombo musicInfo)
            , text <| "\u{3000}最大Score: " ++ String.fromInt (MusicInfo.toMaxScore musicInfo)
            ]
        ]
