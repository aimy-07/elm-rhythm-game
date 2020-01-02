port module UserSetting exposing
    ( UserSetting
    , getUserSetting
    , gotUserSetting
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

import AllMusicData.MusicData.Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)
import UserSetting.Setting as Setting exposing (Setting, SettingDto)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import UserSetting.Setting.Volume exposing (Volume)


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


port getUserSetting : String -> Cmd msg


port gotUserSetting : (SettingDto -> msg) -> Sub msg
