port module Record exposing
    ( Record
    , RecordDto
    , new
    , saveRecord
    , savedRecord
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import Session.User.Uid exposing (Uid)


type alias Record =
    { uid : Uid
    , csvFileName : CsvFileName
    , combo : Int
    , score : Int
    }


type alias RecordDto =
    { uid : String
    , csvFileName : String
    , combo : Int
    , score : Int
    }


new : RecordDto -> Record
new { uid, csvFileName, combo, score } =
    { uid = uid
    , csvFileName = csvFileName
    , combo = combo
    , score = score
    }


port saveRecord : RecordDto -> Cmd msg


port savedRecord : (Bool -> msg) -> Sub msg
