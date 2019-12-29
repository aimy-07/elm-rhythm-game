module Setting exposing (Setting, SettingDto, empty, new)

import Constants
    exposing
        ( bgmVolumeDefault
        , currentModeDefault
        , currentMusicIdDefault
        , notesSpeedDefault
        , seVolumeDefault
        )
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import Setting.NotesSpeed exposing (NotesSpeed)
import Setting.Volume exposing (Volume)


type alias Setting =
    { currentMusicId : MusicId
    , currentMode : Mode
    , notesSpeed : NotesSpeed
    , bgmVolume : Volume
    , seVolume : Volume
    }


type alias SettingDto =
    { currentMusicId : Maybe String
    , currentMode : Maybe String
    , notesSpeed : Maybe Float
    , bgmVolume : Maybe Float
    , seVolume : Maybe Float
    }


new : SettingDto -> Setting
new { currentMusicId, currentMode, notesSpeed, bgmVolume, seVolume } =
    { currentMusicId =
        currentMusicId
            |> Maybe.map identity
            |> Maybe.withDefault currentMusicIdDefault
    , currentMode =
        currentMode
            |> Maybe.map Mode.new
            |> Maybe.withDefault currentModeDefault
    , notesSpeed =
        notesSpeed
            |> Maybe.map identity
            |> Maybe.withDefault notesSpeedDefault
    , bgmVolume =
        bgmVolume
            |> Maybe.map identity
            |> Maybe.withDefault bgmVolumeDefault
    , seVolume =
        seVolume
            |> Maybe.map identity
            |> Maybe.withDefault seVolumeDefault
    }


empty : Setting
empty =
    { currentMusicId = currentMusicIdDefault
    , currentMode = currentModeDefault
    , notesSpeed = notesSpeedDefault
    , bgmVolume = bgmVolumeDefault
    , seVolume = seVolumeDefault
    }
