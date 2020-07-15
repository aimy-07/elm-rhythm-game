module AllMusicData.MusicData.CsvFileName exposing (CsvFileName, create)

import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)


type alias CsvFileName =
    String


create : MusicId -> Mode -> CsvFileName
create musicId mode =
    musicId ++ "_" ++ Mode.unwrap mode
