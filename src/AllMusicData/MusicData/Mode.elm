module AllMusicData.MusicData.Mode exposing (Mode(..), new, toString, unwrap)

import String.Extra as ExString


type Mode
    = Easy
    | Normal
    | Hard


new : String -> Mode
new rawMode =
    case rawMode of
        "easy" ->
            Easy

        "normal" ->
            Normal

        "hard" ->
            Hard

        _ ->
            -- ここに入ることは仕様上ないので考慮しない。型合わせのためEasyにしておく。
            Easy


unwrap : Mode -> String
unwrap mode =
    case mode of
        Easy ->
            "easy"

        Normal ->
            "normal"

        Hard ->
            "hard"


toString : Mode -> String
toString mode =
    unwrap mode
        |> ExString.toTitleCase
