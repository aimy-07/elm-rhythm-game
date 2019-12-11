module Page.Home.AllMusicInfoList exposing (AllMusicInfoList, create, filteredMusicInfoListByMode, init, isLoaded, toMusicInfoList)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MusicInfo as MusicInfo exposing (MusicInfo)
import MusicInfo.Mode exposing (Mode)


type AllMusicInfoList
    = NotLoaded
    | Loaded (List MusicInfo)


init : AllMusicInfoList
init =
    NotLoaded


create : List MusicInfo -> AllMusicInfoList
create musicInfos =
    Loaded musicInfos


isLoaded : AllMusicInfoList -> Bool
isLoaded allMusicInfoList =
    case allMusicInfoList of
        Loaded _ ->
            True

        NotLoaded ->
            False


toMusicInfoList : AllMusicInfoList -> List MusicInfo
toMusicInfoList allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList ->
            musicInfoList

        NotLoaded ->
            []


filteredMusicInfoListByMode : Mode -> AllMusicInfoList -> List MusicInfo
filteredMusicInfoListByMode mode allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList ->
            musicInfoList
                |> List.filter
                    (\musicInfo ->
                        MusicInfo.toMode musicInfo == mode
                    )

        NotLoaded ->
            []
