module Page.Play.PlayingMusicInfo exposing (PlayingMusicInfo, init, isLoaded, new, toMusicInfo)

import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)


type PlayingMusicInfo
    = NotLoaded
    | Loaded MusicInfo


init : PlayingMusicInfo
init =
    NotLoaded


new : MusicInfoDto -> PlayingMusicInfo
new musicInfoDto =
    MusicInfo.new musicInfoDto
        |> Loaded


isLoaded : PlayingMusicInfo -> Bool
isLoaded playingMusicInfo =
    case playingMusicInfo of
        Loaded _ ->
            True

        NotLoaded ->
            False


toMusicInfo : PlayingMusicInfo -> MusicInfo
toMusicInfo playingMusicInfo =
    case playingMusicInfo of
        Loaded musicInfo ->
            musicInfo

        NotLoaded ->
            MusicInfo.empty
