module User exposing (User, UserDto, new, toPictureUrl, toUserName)


type User
    = User
        { uid : String
        , userName : String
        , pictureUrl : String
        }


type alias UserDto =
    { uid : String
    , userName : String
    , pictureUrl : String
    }


new : UserDto -> User
new { uid, userName, pictureUrl } =
    User
        { uid = uid
        , userName = userName
        , pictureUrl = pictureUrl
        }


toUid : User -> String
toUid (User { uid }) =
    uid


toUserName : User -> String
toUserName (User { userName }) =
    userName


toPictureUrl : User -> String
toPictureUrl (User { pictureUrl }) =
    pictureUrl
