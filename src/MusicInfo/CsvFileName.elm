module MusicInfo.CsvFileName exposing (CsvFileName, new, toAudioFileName)

import Constants exposing (currentMusicIdDefault)
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)


type alias CsvFileName =
    String


new : MusicId -> Mode -> CsvFileName
new musicId mode =
    musicId ++ "-" ++ Mode.unwrap mode


toAudioFileName : CsvFileName -> MusicId
toAudioFileName csvFileName =
    String.split "-" csvFileName
        |> List.head
        |> Maybe.withDefault currentMusicIdDefault
