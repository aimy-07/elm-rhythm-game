module Page.Play.CurrentMusicTime exposing (CurrentMusicTime, updateCurrentMusicTime)


type alias CurrentMusicTime =
    Float


updateCurrentMusicTime : CurrentMusicTime -> CurrentMusicTime
updateCurrentMusicTime currentMusicTime =
    currentMusicTime + 10
