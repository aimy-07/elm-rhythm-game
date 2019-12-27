module UserSetting exposing
    ( UserSetting
    , UserSettingData
    , UserSettingDto
    , init
    , isLoaded
    , new
    , toMaybe
    , updateBgmVolume
    , updateCurrentMode
    , updateCurrentMusicId
    , updateNotesSpeed
    , updateSeVolume
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
import UserSetting.NotesSpeed exposing (NotesSpeed)
import UserSetting.Volume exposing (Volume)


type UserSetting
    = Loaded UserSettingData
    | NotLoaded


type alias UserSettingData =
    { currentMusicId : MusicId
    , currentMode : Mode
    , notesSpeed : NotesSpeed
    , bgmVolume : Volume
    , seVolume : Volume
    }


type alias UserSettingDto =
    { currentMusicId : Maybe String
    , currentMode : Maybe String
    , notesSpeed : Maybe Float
    , bgmVolume : Maybe Float
    , seVolume : Maybe Float
    }


init : UserSetting
init =
    NotLoaded


new : UserSettingDto -> UserSetting
new { currentMusicId, currentMode, notesSpeed, bgmVolume, seVolume } =
    Loaded
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


isLoaded : UserSetting -> Bool
isLoaded userSetting =
    case userSetting of
        Loaded _ ->
            True

        NotLoaded ->
            False


toMaybe : UserSetting -> Maybe UserSettingData
toMaybe userSetting =
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
