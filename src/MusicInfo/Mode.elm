module MusicInfo.Mode exposing (Mode, hard, invalid, master, new, normal, toString, unwrap)

import String.Extra as ExString


type Mode
    = Normal
    | Hard
    | Master
    | Invalid


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
            Invalid


unwrap : Mode -> String
unwrap mode =
    case mode of
        Normal ->
            "normal"

        Hard ->
            "hard"

        Master ->
            "master"

        Invalid ->
            ""


toString : Mode -> String
toString mode =
    unwrap mode
        |> ExString.toTitleCase


normal : Mode
normal =
    Normal


hard : Mode
hard =
    Hard


master : Mode
master =
    Master


invalid : Mode
invalid =
    Invalid
