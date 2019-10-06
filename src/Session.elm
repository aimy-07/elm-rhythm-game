module Session exposing (Session, fromUser, init, toNavKey, toUser)

import Browser.Navigation as Nav
import User exposing (User, UserDto)


type Session
    = LoggedIn Nav.Key User
    | NotLogin Nav.Key


toUser : Session -> Maybe User
toUser session =
    case session of
        LoggedIn _ user ->
            Just user

        NotLogin _ ->
            Nothing


toNavKey : Session -> Nav.Key
toNavKey session =
    case session of
        LoggedIn key _ ->
            key

        NotLogin key ->
            key


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
            LoggedIn key user

        Nothing ->
            NotLogin key
