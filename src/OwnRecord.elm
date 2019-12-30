port module OwnRecord exposing
    ( OwnRecord
    , OwnRecordDto
    , findByCsvFileName
    , getOwnRecords
    , gotOwnRecords
    , new
    , toPlayCount
    , toStringCombo
    , toStringComboRank
    , toStringScore
    , toStringScoreRank
    )

import MusicInfo.CsvFileName exposing (CsvFileName)
import Rank


type alias OwnRecord =
    { csvFileName : CsvFileName
    , bestCombo : Int
    , bestScore : Int
    , playCount : Int
    }


type alias OwnRecordDto =
    { csvFileName : String
    , bestCombo : Int
    , bestScore : Int
    , playCount : Int
    }


new : OwnRecordDto -> OwnRecord
new { csvFileName, bestCombo, bestScore, playCount } =
    { csvFileName = csvFileName
    , bestCombo = bestCombo
    , bestScore = bestScore
    , playCount = playCount
    }


findByCsvFileName : CsvFileName -> List OwnRecord -> Maybe OwnRecord
findByCsvFileName csvFileName ownRecords =
    ownRecords
        |> List.filter (.csvFileName >> (==) csvFileName)
        |> List.head


toStringComboRank : Maybe OwnRecord -> Int -> String
toStringComboRank maybeOwnRecord maxCombo =
    maybeOwnRecord
        |> Maybe.map (\record -> Rank.newComboRank record.bestCombo maxCombo)
        |> Maybe.map Rank.toString
        |> Maybe.withDefault "---"


toStringCombo : Maybe OwnRecord -> String
toStringCombo maybeOwnRecord =
    maybeOwnRecord
        |> Maybe.map (.bestCombo >> String.fromInt)
        |> Maybe.withDefault "---"


toStringScoreRank : Maybe OwnRecord -> Int -> String
toStringScoreRank maybeOwnRecord maxScore =
    maybeOwnRecord
        |> Maybe.map (\record -> Rank.newScoreRank record.bestScore maxScore)
        |> Maybe.map Rank.toString
        |> Maybe.withDefault "---"


toStringScore : Maybe OwnRecord -> String
toStringScore maybeOwnRecord =
    maybeOwnRecord
        |> Maybe.map (.bestScore >> String.fromInt)
        |> Maybe.withDefault "---"


toPlayCount : Maybe OwnRecord -> String
toPlayCount maybeOwnRecord =
    maybeOwnRecord
        |> Maybe.map .playCount
        |> Maybe.withDefault 0
        |> String.fromInt


port getOwnRecords : String -> Cmd msg


port gotOwnRecords : (List OwnRecordDto -> msg) -> Sub msg
