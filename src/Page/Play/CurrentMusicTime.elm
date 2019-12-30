port module Page.Play.CurrentMusicTime exposing
    ( CurrentMusicTime
    , getCurrentMusicTime
    , gotCurrentMusicTime
    )


type alias CurrentMusicTime =
    Float


port getCurrentMusicTime : () -> Cmd msg


port gotCurrentMusicTime : (Float -> msg) -> Sub msg
