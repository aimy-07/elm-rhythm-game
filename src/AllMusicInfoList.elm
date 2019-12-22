module AllMusicInfoList exposing
    ( AllMusicInfoList
    , filterByMode
    , findByCsvFileName
    , init
    , isLoaded
    , new
    , toMusicInfoList
    )

import MusicInfo exposing (MusicInfo)
import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Mode exposing (Mode)


type AllMusicInfoList
    = Loaded (List MusicInfo)
    | NotLoaded


init : AllMusicInfoList
init =
    NotLoaded


new : List MusicInfo -> AllMusicInfoList
new musicInfos =
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
