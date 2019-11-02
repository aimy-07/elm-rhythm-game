module MusicInfo.CsvFileName exposing (CsvFileName, toAudioFileName)


type alias CsvFileName =
    String


toAudioFileName : CsvFileName -> String
toAudioFileName csvFileName =
    String.split "-" csvFileName
        |> List.head
        |> Maybe.withDefault ""
