module UserSetting exposing (UserSetting, UserSettingData, UserSettingDto, init, isLoaded, new, toMaybe, updateNotesSpeed)

import Constants exposing (notesSpeedDefault)
import UserSetting.NotesSpeed exposing (NotesSpeed)


type UserSetting
    = Loaded UserSettingData
    | NotLoaded


type alias UserSettingData =
    { notesSpeed : NotesSpeed
    }


type alias UserSettingDto =
    { notesSpeed : Maybe NotesSpeed
    }


init : UserSetting
init =
    NotLoaded


new : UserSettingDto -> UserSetting
new { notesSpeed } =
    Loaded
        { notesSpeed =
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


updateNotesSpeed : NotesSpeed -> UserSetting -> UserSetting
updateNotesSpeed notesSpeed userSetting =
    case userSetting of
        Loaded setting ->
            Loaded
                { setting | notesSpeed = notesSpeed }

        NotLoaded ->
            userSetting
