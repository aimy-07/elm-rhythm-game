port module Setting exposing
    ( Setting
    , SettingDto
    , empty
    , new
    , saveBgmVolume
    , saveCurrentMode
    , saveCurrentMusicId
    , saveNotesSpeed
    , saveSeVolume
    )

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


port saveCurrentMusicId : { uid : String, currentMusicId : String } -> Cmd msg


port saveCurrentMode : { uid : String, currentMode : String } -> Cmd msg


port saveNotesSpeed : { uid : String, notesSpeed : Float } -> Cmd msg


port saveBgmVolume : { uid : String, bgmVolume : Float } -> Cmd msg


port saveSeVolume : { uid : String, seVolume : Float } -> Cmd msg
