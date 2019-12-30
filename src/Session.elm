port module Session exposing
    ( Session
    , canceledSignIn
    , init
    , isLoggedIn
    , onAuthStateChanged
    , signInWithGithub
    , signInWithGoogle
    , signInWithTwitter
    , signOut
    , toLoggedIn
    , toNavKey
    , toUser
    , updateUser
    )

import Browser.Navigation as Nav
import User exposing (User, UserDto)


type Session
    = LoggedIn Nav.Key User
    | NotLogin Nav.Key


init : Nav.Key -> Session
init key =
    NotLogin key


toLoggedIn : Nav.Key -> Maybe UserDto -> Session
toLoggedIn key maybeUserDto =
    case maybeUserDto of
        Just userDto ->
            LoggedIn key (User.new userDto)

        Nothing ->
            NotLogin key


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        NotLogin _ ->
            False


toNavKey : Session -> Nav.Key
toNavKey session =
    case session of
        LoggedIn key _ ->
            key

        NotLogin key ->
            key


toUser : Session -> Maybe User
toUser session =
    case session of
        LoggedIn _ user ->
            Just user

        NotLogin _ ->
            Nothing


updateUser : data -> (data -> User -> User) -> Session -> Session
updateUser data update session =
    case session of
        LoggedIn key user ->
            LoggedIn key (update data user)

        NotLogin _ ->
            session


port onAuthStateChanged : (Maybe UserDto -> msg) -> Sub msg


port signInWithGoogle : () -> Cmd msg


port signInWithTwitter : () -> Cmd msg


port signInWithGithub : () -> Cmd msg


port canceledSignIn : (() -> msg) -> Sub msg


port signOut : () -> Cmd msg
