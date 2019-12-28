module AllMusicInfoList exposing
    ( AllMusicInfoList
    , filterByMode
    , findByCsvFileName
    , init
    , isLoaded
    , new
    , ready
    , toMusicInfoList
    )

import MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Mode exposing (Mode)


type AllMusicInfoList
    = Loaded (List MusicInfo) Bool
    | NotLoaded


init : AllMusicInfoList
init =
    NotLoaded


new : List MusicInfoDto -> AllMusicInfoList
new musicInfoDtos =
    Loaded (List.map MusicInfo.new musicInfoDtos) False


ready : AllMusicInfoList -> AllMusicInfoList
ready allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList _ ->
            Loaded musicInfoList True

        NotLoaded ->
            NotLoaded


isLoaded : AllMusicInfoList -> Bool
isLoaded allMusicInfoList =
    case allMusicInfoList of
        Loaded _ isReady ->
            isReady

        NotLoaded ->
            False


toMusicInfoList : AllMusicInfoList -> List MusicInfo
toMusicInfoList allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList isReady ->
            if isReady then
                musicInfoList

            else
                []

        NotLoaded ->
            []


filterByMode : Mode -> AllMusicInfoList -> List MusicInfo
filterByMode mode allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList isReady ->
            if isReady then
                musicInfoList
                    |> List.filter (.mode >> (==) mode)

            else
                []

        NotLoaded ->
            []


findByCsvFileName : CsvFileName -> AllMusicInfoList -> Maybe MusicInfo
findByCsvFileName csvFileName allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList isReady ->
            if isReady then
                musicInfoList
                    |> List.filter (.csvFileName >> (==) csvFileName)
                    |> List.head

            else
                Nothing

        NotLoaded ->
            Nothing
