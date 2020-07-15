port module OwnRecord exposing
    ( OwnRecord
    , OwnRecordDto
    , getOwnRecord
    , gotOwnRecord
    , new
    , saveOwnRecord
    , savedOwnRecord
    , toBestCombo
    , toBestScore
    , toCsvFileName
    , toPlayCount
    , toUid
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import Session.User.Uid exposing (Uid)


type OwnRecord
    = OwnRecord
        { uid : Uid
        , csvFileName : CsvFileName
        , bestCombo : Int
        , bestScore : Int
        , playCount : Int
        }


type alias OwnRecordDto =
    { uid : String
    , csvFileName : String
    , playRecord : Maybe PlayRecordDto
    }


type alias PlayRecordDto =
    { bestCombo : Int
    , bestScore : Int
    , playCount : Int
    }


toUid : OwnRecord -> Uid
toUid (OwnRecord { uid }) =
    uid


toCsvFileName : OwnRecord -> CsvFileName
toCsvFileName (OwnRecord { csvFileName }) =
    csvFileName


toBestCombo : OwnRecord -> Int
toBestCombo (OwnRecord { bestCombo }) =
    bestCombo


toBestScore : OwnRecord -> Int
toBestScore (OwnRecord { bestScore }) =
    bestScore


toPlayCount : OwnRecord -> Int
toPlayCount (OwnRecord { playCount }) =
    playCount


new : OwnRecordDto -> OwnRecord
new { uid, csvFileName, playRecord } =
    OwnRecord
        { uid = uid
        , csvFileName = csvFileName
        , bestCombo =
            playRecord
                |> Maybe.map .bestCombo
                |> Maybe.withDefault 0
        , bestScore =
            playRecord
                |> Maybe.map .bestScore
                |> Maybe.withDefault 0
        , playCount =
            playRecord
                |> Maybe.map .playCount
                |> Maybe.withDefault 0
        }


saveOwnRecord :
    { uid : Uid
    , csvFileName : CsvFileName
    , combo : Int
    , bestCombo : Int
    , score : Int
    , bestScore : Int
    , playCount : Int
    }
    -> Cmd msg
saveOwnRecord { uid, csvFileName, combo, bestCombo, score, bestScore, playCount } =
    saveOwnRecord_
        { uid = uid
        , csvFileName = csvFileName
        , playRecord =
            Just
                { bestCombo = Basics.max combo bestCombo
                , bestScore = Basics.max score bestScore
                , playCount = playCount + 1
                }
        }


port getOwnRecord : { uid : String, csvFileName : String } -> Cmd msg


port gotOwnRecord : (OwnRecordDto -> msg) -> Sub msg


port saveOwnRecord_ : OwnRecordDto -> Cmd msg


port savedOwnRecord : (() -> msg) -> Sub msg
