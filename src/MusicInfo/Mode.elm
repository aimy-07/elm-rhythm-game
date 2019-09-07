module MusicInfo.Mode exposing (Mode, hard, isInvalid, master, new, normal, unwrap)


type Mode
    = Normal
    | Hard
    | Master
    | Invalid


new : String -> Mode
new rawMode =
    case rawMode of
        "Normal" ->
            Normal

        "Hard" ->
            Hard

        "Master" ->
            Master

        _ ->
            Invalid


unwrap : Mode -> String
unwrap mode =
    case mode of
        Normal ->
            "Normal"

        Hard ->
            "Hard"

        Master ->
            "Master"

        Invalid ->
            ""


normal : Mode
normal =
    Normal


hard : Mode
hard =
    Hard


master : Mode
master =
    Master


isInvalid : Mode -> Bool
isInvalid mode =
    mode == Invalid
