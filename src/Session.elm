module Session exposing
    ( Session
    , init
    , isLoaded
    , isLoggedIn
    , resetUserSetting
    , setAllMusicInfoList
    , setUser
    , setUserSetting
    , toAllMusicInfoList
    , toNavKey
    , toUser
    , toUserSetting
    , updateUser
    , updateUserSetting
    )

import AllMusicInfoList exposing (AllMusicInfoList)
import Browser.Navigation as Nav
import MusicInfo as MusicInfo exposing (MusicInfoDto)
import User exposing (User, UserDto)
import UserSetting exposing (UserSetting, UserSettingDto)


type Session
    = LoggedIn Nav.Key User UserSetting AllMusicInfoList
    | NotLogin Nav.Key


toNavKey : Session -> Nav.Key
toNavKey session =
    case session of
        LoggedIn key _ _ _ ->
            key

        NotLogin key ->
            key


toUser : Session -> Maybe User
toUser session =
    case session of
        LoggedIn _ user _ _ ->
            Just user

        NotLogin _ ->
            Nothing


toUserSetting : Session -> UserSetting
toUserSetting session =
    case session of
        LoggedIn _ _ userSetting _ ->
            userSetting

        NotLogin _ ->
            UserSetting.init


toAllMusicInfoList : Session -> AllMusicInfoList
toAllMusicInfoList session =
    case session of
        LoggedIn _ _ _ allMusicInfoList ->
            allMusicInfoList

        NotLogin _ ->
            AllMusicInfoList.init


init : Nav.Key -> Session
init key =
    NotLogin key


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ _ _ _ ->
            True

        NotLogin _ ->
            False


isLoaded : Session -> Bool
isLoaded session =
    case session of
        LoggedIn _ _ userSetting allMusicInfoList ->
            UserSetting.isLoaded userSetting && AllMusicInfoList.isLoaded allMusicInfoList

        NotLogin _ ->
            False


setUser : Nav.Key -> Maybe UserDto -> Session
setUser key maybeUserDto =
    case maybeUserDto of
        Just userDto ->
            let
                user =
                    User.new userDto
            in
            LoggedIn key user UserSetting.init AllMusicInfoList.init

        Nothing ->
            NotLogin key


updateUser : data -> (data -> User -> User) -> Session -> Session
updateUser data update session =
    case session of
        LoggedIn key user userSetting allMusicInfoList ->
            LoggedIn key (update data user) userSetting allMusicInfoList

        NotLogin _ ->
            session


setUserSetting : UserSettingDto -> Session -> Session
setUserSetting userSettingDto session =
    case session of
        LoggedIn key user _ allMusicInfoList ->
            let
                userSetting =
                    UserSetting.new userSettingDto
            in
            LoggedIn key user userSetting allMusicInfoList

        NotLogin _ ->
            session


resetUserSetting : Session -> Session
resetUserSetting session =
    case session of
        LoggedIn key user _ allMusicInfoList ->
            LoggedIn key user UserSetting.init allMusicInfoList

        NotLogin _ ->
            session


updateUserSetting : data -> (data -> UserSetting -> UserSetting) -> Session -> Session
updateUserSetting data update session =
    case session of
        LoggedIn key user userSetting allMusicInfoList ->
            LoggedIn key user (update data userSetting) allMusicInfoList

        NotLogin _ ->
            session


setAllMusicInfoList : List MusicInfoDto -> Session -> Session
setAllMusicInfoList musicInfoDtos session =
    case session of
        LoggedIn key user userSetting _ ->
            let
                allMusicInfoList =
                    musicInfoDtos
                        |> List.map MusicInfo.new
                        |> AllMusicInfoList.new
            in
            LoggedIn key user userSetting allMusicInfoList

        NotLogin _ ->
            session
