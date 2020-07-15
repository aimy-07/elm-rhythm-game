port module Page.Play.Result exposing
    ( Result
    , ResultDto
    , isBestCombo
    , isBestScore
    , isFullCombo
    , new
    , saveResult
    , savedResult
    , toCombo
    , toCreatedAt
    , toCsvFileName
    , toScore
    , toUid
    , updateIsBest
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import Session.User.Uid exposing (Uid)


type Result
    = Result
        { uid : Uid
        , csvFileName : CsvFileName
        , combo : Int
        , score : Int
        , createdAt : CreatedAt
        , isBestCombo : Bool
        , isBestScore : Bool
        , isFullCombo : Bool
        }


type alias CreatedAt =
    Float


type alias ResultDto =
    { uid : String
    , csvFileName : String
    , combo : Int
    , score : Int
    , createdAt : Float
    }


toUid : Result -> Uid
toUid (Result { uid }) =
    uid


toCsvFileName : Result -> CsvFileName
toCsvFileName (Result { csvFileName }) =
    csvFileName


toCombo : Result -> Int
toCombo (Result { combo }) =
    combo


toScore : Result -> Int
toScore (Result { score }) =
    score


toCreatedAt : Result -> CreatedAt
toCreatedAt (Result { createdAt }) =
    createdAt


isBestCombo : Result -> Bool
isBestCombo (Result result) =
    result.isBestCombo


isBestScore : Result -> Bool
isBestScore (Result result) =
    result.isBestScore


isFullCombo : Result -> Bool
isFullCombo (Result result) =
    result.isFullCombo


new : ResultDto -> Int -> Result
new { uid, csvFileName, combo, score, createdAt } maxCombo =
    Result
        { uid = uid
        , csvFileName = csvFileName
        , combo = combo
        , score = score
        , createdAt = createdAt
        , isBestCombo = False
        , isBestScore = False
        , isFullCombo = combo == maxCombo
        }


updateIsBest : Int -> Int -> Result -> Result
updateIsBest bestCombo bestScore (Result result) =
    Result
        { result
            | isBestCombo = result.combo > bestCombo
            , isBestScore = result.score > bestScore
        }


saveResult : Result -> Cmd msg
saveResult (Result result) =
    saveResult_
        { uid = result.uid
        , csvFileName = result.csvFileName
        , combo = result.combo
        , score = result.score
        , createdAt = result.createdAt
        }


port saveResult_ : ResultDto -> Cmd msg


port savedResult : (() -> msg) -> Sub msg
