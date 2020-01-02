module AllMusicData.MusicData.Level exposing (Level, toString)


type alias Level =
    Int


toString : Level -> String
toString level =
    String.repeat level "◆" ++ String.repeat (8 - level) "◇"
