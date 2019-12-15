module Session exposing (Session, fromUser, init, toAllMusicInfoList, toNavKey, toUser, updateAllMusicInfoList)

import AllMusicInfoList exposing (AllMusicInfoList)
import Browser.Navigation as Nav
import MusicInfo as MusicInfo exposing (MusicInfoDto)
import User exposing (User, UserDto)


type Session
    = LoggedIn Nav.Key User AllMusicInfoList
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


toAllMusicInfoList : Session -> AllMusicInfoList
toAllMusicInfoList session =
    case session of
        LoggedIn _ _ allMusicInfoList ->
            allMusicInfoList

        NotLogin _ ->
            AllMusicInfoList.init


init : Nav.Key -> Session
init key =
    NotLogin key


fromUser : Nav.Key -> Maybe UserDto -> Session
fromUser key maybeUserDto =
    case maybeUserDto of
        Just userDto ->
            let
                user =
                    User.new userDto
            in
            LoggedIn key user AllMusicInfoList.init

        Nothing ->
            NotLogin key


updateAllMusicInfoList : List MusicInfoDto -> Session -> Session
updateAllMusicInfoList musicInfoDtos session =
    case session of
        LoggedIn key user _ ->
            let
                allMusicInfoList =
                    musicInfoDtos
                        |> List.map MusicInfo.new
                        |> AllMusicInfoList.create
            in
            LoggedIn key user allMusicInfoList

        NotLogin _ ->
            session
