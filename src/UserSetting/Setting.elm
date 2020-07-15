port module UserSetting.Setting exposing
    ( Setting
    , SettingDto
    , empty
    , new
    , saveBgmVolume
    , saveCurrentMode
    , saveCurrentMusicId
    , saveNotesSpeed
    , saveSeVolume
    , toBgmVolume
    , toCurrentMode
    , toCurrentMusicId
    , toNotesSpeed
    , toSeVolume
    , updateBgmVolume
    , updateCurrentMode
    , updateCurrentMusicId
    , updateNotesSpeed
    , updateSeVolume
    )

import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)
import Constants
    exposing
        ( bgmVolumeDefault
        , currentModeDefault
        , currentMusicIdDefault
        , notesSpeedDefault
        , seVolumeDefault
        )
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import UserSetting.Setting.Volume exposing (Volume)


type Setting
    = Setting
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


toCurrentMusicId : Setting -> MusicId
toCurrentMusicId (Setting { currentMusicId }) =
    currentMusicId


toCurrentMode : Setting -> Mode
toCurrentMode (Setting { currentMode }) =
    currentMode


toNotesSpeed : Setting -> NotesSpeed
toNotesSpeed (Setting { notesSpeed }) =
    notesSpeed


toBgmVolume : Setting -> Volume
toBgmVolume (Setting { bgmVolume }) =
    bgmVolume


toSeVolume : Setting -> Volume
toSeVolume (Setting { seVolume }) =
    seVolume


new : SettingDto -> Setting
new { currentMusicId, currentMode, notesSpeed, bgmVolume, seVolume } =
    Setting
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
    Setting
        { currentMusicId = currentMusicIdDefault
        , currentMode = currentModeDefault
        , notesSpeed = notesSpeedDefault
        , bgmVolume = bgmVolumeDefault
        , seVolume = seVolumeDefault
        }


updateCurrentMusicId : MusicId -> Setting -> Setting
updateCurrentMusicId currentMusicId (Setting setting) =
    Setting { setting | currentMusicId = currentMusicId }


updateCurrentMode : Mode -> Setting -> Setting
updateCurrentMode currentMode (Setting setting) =
    Setting { setting | currentMode = currentMode }


updateNotesSpeed : NotesSpeed -> Setting -> Setting
updateNotesSpeed notesSpeed (Setting setting) =
    Setting { setting | notesSpeed = notesSpeed }


updateBgmVolume : Volume -> Setting -> Setting
updateBgmVolume bgmVolume (Setting setting) =
    Setting { setting | bgmVolume = bgmVolume }


updateSeVolume : Volume -> Setting -> Setting
updateSeVolume seVolume (Setting setting) =
    Setting { setting | seVolume = seVolume }


port saveCurrentMusicId : { uid : String, currentMusicId : String } -> Cmd msg


port saveCurrentMode : { uid : String, currentMode : String } -> Cmd msg


port saveNotesSpeed : { uid : String, notesSpeed : Float } -> Cmd msg


port saveBgmVolume : { uid : String, bgmVolume : Float } -> Cmd msg


port saveSeVolume : { uid : String, seVolume : Float } -> Cmd msg
