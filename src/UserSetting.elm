port module UserSetting exposing
    ( UserSetting
    , UserSettingDto
    , getUserSetting
    , gotUserSetting
    , init
    , isLoaded
    , new
    , toSetting
    , updateSetting
    )

import UserSetting.Setting as Setting exposing (Setting, SettingDto)


type UserSetting
    = Loaded Setting
    | NotLoaded


type alias UserSettingDto =
    Maybe SettingDto


init : UserSetting
init =
    NotLoaded


new : UserSettingDto -> UserSetting
new userSettingDto =
    userSettingDto
        |> Maybe.map Setting.new
        |> Maybe.withDefault Setting.empty
        |> Loaded


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


updateSetting : (Setting -> Setting) -> UserSetting -> UserSetting
updateSetting update userSetting =
    case userSetting of
        Loaded setting ->
            Loaded (update setting)

        NotLoaded ->
            userSetting


port getUserSetting : String -> Cmd msg


port gotUserSetting : (UserSettingDto -> msg) -> Sub msg
