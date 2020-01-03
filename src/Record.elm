port module Record exposing
    ( Record
    , RecordDto
    , new
    , saveRecord
    , savedRecord
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import CreatedAt exposing (CreatedAt)
import Session.User.Uid exposing (Uid)


type alias Record =
    { uid : Uid
    , csvFileName : CsvFileName
    , combo : Int
    , score : Int
    , createdAt : CreatedAt
    }


type alias RecordDto =
    { uid : String
    , csvFileName : String
    , combo : Int
    , score : Int
    , createdAt : Float
    }


new : RecordDto -> Record
new { uid, csvFileName, combo, score, createdAt } =
    { uid = uid
    , csvFileName = csvFileName
    , combo = combo
    , score = score
    , createdAt = createdAt
    }


saveRecord : Record -> Cmd msg
saveRecord record =
    saveRecord_
        { uid = record.uid
        , csvFileName = record.csvFileName
        , combo = record.combo
        , score = record.score
        , createdAt = record.createdAt
        }


port saveRecord_ : RecordDto -> Cmd msg


port savedRecord : (() -> msg) -> Sub msg
