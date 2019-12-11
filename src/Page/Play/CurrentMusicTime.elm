module Page.Play.CurrentMusicTime exposing (CurrentMusicTime, update)


type alias CurrentMusicTime =
    Float


update : CurrentMusicTime -> CurrentMusicTime
update currentMusicTime =
    currentMusicTime + 10
