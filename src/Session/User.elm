port module Session.User exposing
    ( User
    , UserDto
    , completedSaveUserPicture
    , failedSaveUserPicture
    , getUsers
    , gotUsers
    , new
    , saveUserName
    , saveUserPicture
    , toName
    , toPictureUrl
    , toUid
    , updateName
    , updatePictureUrl
    )

import Json.Decode as Decode
import Session.User.Uid exposing (Uid)


type User
    = User
        { uid : Uid
        , name : String
        , pictureUrl : String
        }


type alias UserDto =
    { uid : String
    , name : String
    , pictureUrl : String
    }


toUid : User -> Uid
toUid (User { uid }) =
    uid


toName : User -> Uid
toName (User { name }) =
    name


toPictureUrl : User -> String
toPictureUrl (User { pictureUrl }) =
    pictureUrl


new : UserDto -> User
new { uid, name, pictureUrl } =
    User
        { uid = uid
        , name = name
        , pictureUrl = pictureUrl
        }


updateName : String -> User -> User
updateName name (User user) =
    User { user | name = name }


updatePictureUrl : String -> User -> User
updatePictureUrl pictureUrl (User user) =
    User { user | pictureUrl = pictureUrl }


port getUsers : List String -> Cmd msg


port gotUsers : (List UserDto -> msg) -> Sub msg


port saveUserName : { uid : String, name : String } -> Cmd msg


port saveUserPicture : { uid : String, event : Decode.Value } -> Cmd msg


port completedSaveUserPicture : (String -> msg) -> Sub msg


port failedSaveUserPicture : (() -> msg) -> Sub msg
