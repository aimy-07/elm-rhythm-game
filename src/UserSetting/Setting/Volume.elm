module UserSetting.Setting.Volume exposing (Volume, fromString)


type alias Volume =
    Float


fromString : String -> Volume
fromString str =
    String.toFloat str
        |> Maybe.map identity
        |> Maybe.withDefault 0
