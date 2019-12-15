module AllMusicInfoList exposing
    ( AllMusicInfoList
    , create
    , filterByMode
    , findByCsvFileName
    , init
    , isLoaded
    , toMusicInfoList
    )

import MusicInfo exposing (MusicInfo)
import MusicInfo.CsvFileName exposing (CsvFileName)
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


filterByMode : Mode -> AllMusicInfoList -> List MusicInfo
filterByMode mode allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList ->
            musicInfoList
                |> List.filter (.mode >> (==) mode)

        NotLoaded ->
            []


findByCsvFileName : CsvFileName -> AllMusicInfoList -> Maybe MusicInfo
findByCsvFileName csvFileName allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList ->
            musicInfoList
                |> List.filter (.csvFileName >> (==) csvFileName)
                |> List.head

        NotLoaded ->
            Nothing
