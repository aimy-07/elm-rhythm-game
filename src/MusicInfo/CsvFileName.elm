module MusicInfo.CsvFileName exposing (CsvFileName, new, toMode, toMusicId)

import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)


type alias CsvFileName =
    String


new : MusicId -> Mode -> CsvFileName
new musicId mode =
    musicId ++ "-" ++ Mode.unwrap mode


toMusicId : CsvFileName -> MusicId
toMusicId csvFileName =
    String.split "-" csvFileName
        |> List.head
        |> Maybe.map
            (\name ->
                if name /= "" then
                    name

                else
                    "sample_sound"
            )
        |> Maybe.withDefault "sample_sound"


toMode : CsvFileName -> Mode
toMode csvFileName =
    String.split "-" csvFileName
        |> List.drop 1
        |> List.head
        |> Maybe.map Mode.new
        |> Maybe.withDefault Mode.normal
