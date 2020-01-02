module AllMusicData.MusicData.CsvFileName exposing (CsvFileName, new)

import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)


type alias CsvFileName =
    String


new : MusicId -> Mode -> CsvFileName
new musicId mode =
    musicId ++ "_" ++ Mode.unwrap mode
