module AllMusicData.MusicData.Mode exposing (Mode(..), new, toString, unwrap)

import String.Extra as ExString


type Mode
    = Normal
    | Hard
    | Master


new : String -> Mode
new rawMode =
    case rawMode of
        "normal" ->
            Normal

        "hard" ->
            Hard

        "master" ->
            Master

        _ ->
            -- ここに入ることは仕様上ないので考慮しない。型合わせのためNormalにしておく。
            Normal


unwrap : Mode -> String
unwrap mode =
    case mode of
        Normal ->
            "normal"

        Hard ->
            "hard"

        Master ->
            "master"


toString : Mode -> String
toString mode =
    unwrap mode
        |> ExString.toTitleCase
