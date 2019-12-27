module UserSetting.NotesSpeed exposing (NotesSpeed, fromString)


type alias NotesSpeed =
    Float


fromString : String -> NotesSpeed
fromString str =
    String.toFloat str
        |> Maybe.map identity
        |> Maybe.withDefault 0
