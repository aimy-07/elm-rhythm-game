module UserSetting exposing
    ( UserSetting
    , UserSettingData
    , UserSettingDto
    , init
    , isLoaded
    , new
    , toMaybe
    , updateCurrentMode
    , updateCurrentMusicId
    , updateNotesSpeed
    )

import Constants exposing (currentModeDefault, currentMusicIdDefault, notesSpeedDefault)
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import UserSetting.NotesSpeed exposing (NotesSpeed)


type UserSetting
    = Loaded UserSettingData
    | NotLoaded


type alias UserSettingData =
    { currentMusicId : MusicId
    , currentMode : Mode
    , notesSpeed : NotesSpeed
    }


type alias UserSettingDto =
    { currentMusicId : Maybe String
    , currentMode : Maybe String
    , notesSpeed : Maybe NotesSpeed
    }


init : UserSetting
init =
    NotLoaded


new : UserSettingDto -> UserSetting
new { currentMusicId, currentMode, notesSpeed } =
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
