module User exposing (Uid, User, UserDto, new, updatePictureUrl, updateUserName)


type alias Uid =
    String


type alias User =
    { uid : Uid
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
    { uid = uid
    , userName = userName
    , pictureUrl = pictureUrl
    }


updateUserName : String -> User -> User
updateUserName userName user =
    { user | userName = userName }


updatePictureUrl : String -> User -> User
updatePictureUrl pictureUrl user =
    { user | pictureUrl = pictureUrl }
