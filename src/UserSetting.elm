module UserSetting exposing
    ( UserSetting
    , init
    , isLoaded
    , new
    , toSetting
    , updateBgmVolume
    , updateCurrentMode
    , updateCurrentMusicId
    , updateNotesSpeed
    , updateSeVolume
    )

import MusicInfo.Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import Setting exposing (Setting, SettingDto)
import Setting.NotesSpeed exposing (NotesSpeed)
import Setting.Volume exposing (Volume)


type UserSetting
    = Loaded Setting
    | NotLoaded


init : UserSetting
init =
    NotLoaded


new : SettingDto -> UserSetting
new settingDto =
    Loaded <| Setting.new settingDto


isLoaded : UserSetting -> Bool
isLoaded userSetting =
    case userSetting of
        Loaded _ ->
            True

        NotLoaded ->
            False


toSetting : UserSetting -> Maybe Setting
toSetting userSetting =
    case userSetting of
        Loaded setting ->
            Just setting

        NotLoaded ->
            Nothing


updateCurrentMusicId : MusicId -> UserSetting -> UserSetting
updateCurrentMusicId currentMusicId userSetting =
    case userSetting of
        Loaded setting ->
            Loaded
                { setting | currentMusicId = currentMusicId }

        NotLoaded ->
            userSetting


updateCurrentMode : Mode -> UserSetting -> UserSetting
updateCurrentMode currentMode userSetting =
    case userSetting of
        Loaded setting ->
            Loaded
                { setting | currentMode = currentMode }

        NotLoaded ->
            userSetting


updateNotesSpeed : NotesSpeed -> UserSetting -> UserSetting
updateNotesSpeed notesSpeed userSetting =
    case userSetting of
        Loaded setting ->
            Loaded
                { setting | notesSpeed = notesSpeed }

        NotLoaded ->
            userSetting


updateBgmVolume : Volume -> UserSetting -> UserSetting
updateBgmVolume bgmVolume userSetting =
    case userSetting of
        Loaded setting ->
            Loaded
                { setting | bgmVolume = bgmVolume }

        NotLoaded ->
            userSetting


updateSeVolume : Volume -> UserSetting -> UserSetting
updateSeVolume seVolume userSetting =
    case userSetting of
        Loaded setting ->
            Loaded
                { setting | seVolume = seVolume }

        NotLoaded ->
            userSetting
