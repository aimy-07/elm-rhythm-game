module Session exposing
    ( Session
    , init
    , isLoaded
    , isLoggedIn
    , resetUserSetting
    , setUser
    , setUserSetting
    , toNavKey
    , toUser
    , toUserSetting
    , updateUser
    , updateUserSetting
    )

import Browser.Navigation as Nav
import User exposing (User, UserDto)
import UserSetting exposing (UserSetting, UserSettingDto)


type Session
    = LoggedIn Nav.Key User UserSetting
    | NotLogin Nav.Key


toNavKey : Session -> Nav.Key
toNavKey session =
    case session of
        LoggedIn key _ _ ->
            key

        NotLogin key ->
            key


toUser : Session -> Maybe User
toUser session =
    case session of
        LoggedIn _ user _ ->
            Just user

        NotLogin _ ->
            Nothing


toUserSetting : Session -> UserSetting
toUserSetting session =
    case session of
        LoggedIn _ _ userSetting ->
            userSetting

        NotLogin _ ->
            UserSetting.init


init : Nav.Key -> Session
init key =
    NotLogin key


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ _ _ ->
            True

        NotLogin _ ->
            False


isLoaded : Session -> Bool
isLoaded session =
    case session of
        LoggedIn _ _ userSetting ->
            UserSetting.isLoaded userSetting

        NotLogin _ ->
            False


setUser : Nav.Key -> Maybe UserDto -> Session
setUser key maybeUserDto =
    case maybeUserDto of
        Just userDto ->
            LoggedIn key (User.new userDto) UserSetting.init

        Nothing ->
            NotLogin key


updateUser : data -> (data -> User -> User) -> Session -> Session
updateUser data update session =
    case session of
        LoggedIn key user userSetting ->
            LoggedIn key (update data user) userSetting

        NotLogin _ ->
            session


setUserSetting : UserSettingDto -> Session -> Session
setUserSetting userSettingDto session =
    case session of
        LoggedIn key user _ ->
            LoggedIn key user (UserSetting.new userSettingDto)

        NotLogin _ ->
            session


resetUserSetting : Session -> Session
resetUserSetting session =
    case session of
        LoggedIn key user _ ->
            LoggedIn key user UserSetting.init

        NotLogin _ ->
            session


updateUserSetting : data -> (data -> UserSetting -> UserSetting) -> Session -> Session
updateUserSetting data update session =
    case session of
        LoggedIn key user userSetting ->
            LoggedIn key user (update data userSetting)

        NotLogin _ ->
            session
