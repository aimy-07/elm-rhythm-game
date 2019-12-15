module MusicInfo.CsvFileName exposing (CsvFileName, create, toAudioFileName)

import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)


type alias CsvFileName =
    String


toAudioFileName : CsvFileName -> String
toAudioFileName csvFileName =
    String.split "-" csvFileName
        |> List.head
        |> Maybe.withDefault ""


create : MusicId -> Mode -> CsvFileName
create musicId mode =
    musicId ++ "-" ++ Mode.unwrap mode
