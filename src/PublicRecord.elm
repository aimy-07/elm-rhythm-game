module PublicRecord exposing
    ( BestScoreRecord
    , PublicRecord
    , PublicRecordDto
    , findByCsvFileName
    , new
    , toFirstScoreRecord
    , toSecondScoreRecord
    , toStringScore
    , toStringUserName
    , toThirdScoreRecord
    )

import MusicInfo.CsvFileName exposing (CsvFileName)


type alias PublicRecord =
    { csvFileName : CsvFileName
    , bestScores : List BestScoreRecord
    }


type alias BestScoreRecord =
    { userName : String
    , score : Int
    }


type alias PublicRecordDto =
    { csvFileName : String
    , bestScores : List BestScoreRecordDto
    }


type alias BestScoreRecordDto =
    { userName : String
    , score : Int
    }


new : PublicRecordDto -> PublicRecord
new { csvFileName, bestScores } =
    { csvFileName = csvFileName
    , bestScores = bestScores
    }


findByCsvFileName : CsvFileName -> List PublicRecord -> Maybe PublicRecord
findByCsvFileName csvFileName publicRecords =
    publicRecords
        |> List.filter (.csvFileName >> (==) csvFileName)
        |> List.head


toFirstScoreRecord : Maybe PublicRecord -> Maybe BestScoreRecord
toFirstScoreRecord maybePublicRecord =
    maybePublicRecord
        |> Maybe.map (.bestScores >> List.sortBy .score >> List.reverse)
        |> Maybe.withDefault []
        |> List.head


toSecondScoreRecord : Maybe PublicRecord -> Maybe BestScoreRecord
toSecondScoreRecord maybePublicRecord =
    maybePublicRecord
        |> Maybe.map (.bestScores >> List.sortBy .score >> List.reverse >> List.drop 1)
        |> Maybe.withDefault []
        |> List.head


toThirdScoreRecord : Maybe PublicRecord -> Maybe BestScoreRecord
toThirdScoreRecord maybePublicRecord =
    maybePublicRecord
        |> Maybe.map (.bestScores >> List.sortBy .score >> List.reverse >> List.drop 2)
        |> Maybe.withDefault []
        |> List.head


toStringUserName : Maybe BestScoreRecord -> String
toStringUserName maybeRecord =
    maybeRecord
        |> Maybe.map .userName
        |> Maybe.withDefault "---"


toStringScore : Maybe BestScoreRecord -> String
toStringScore maybeRecord =
    maybeRecord
        |> Maybe.map (.score >> String.fromInt)
        |> Maybe.withDefault "---"
