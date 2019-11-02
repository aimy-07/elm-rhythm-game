module Page.Play.CurrentMusicInfo exposing
    ( CurrentMusicInfo
    , init
    , isLoaded
    , new
    , toMusicInfo
    )

import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)


type CurrentMusicInfo
    = NotLoaded
    | Loaded MusicInfo


init : CurrentMusicInfo
init =
    NotLoaded


new : MusicInfoDto -> CurrentMusicInfo
new musicInfoDto =
    MusicInfo.new musicInfoDto
        |> Loaded


isLoaded : CurrentMusicInfo -> Bool
isLoaded currentMusicInfo =
    case currentMusicInfo of
        Loaded _ ->
            True

        NotLoaded ->
            False


toMusicInfo : CurrentMusicInfo -> MusicInfo
toMusicInfo currentMusicInfo =
    case currentMusicInfo of
        Loaded musicInfo ->
            musicInfo

        NotLoaded ->
            MusicInfo.empty
