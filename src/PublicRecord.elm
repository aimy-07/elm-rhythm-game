module PublicRecord exposing
    ( PublicRecord
    , PublicRecordDto
    , new
    , toStringBestScore
    , toStringUserName
    )


type alias PublicRecord =
    { userName : String
    , bestScore : Int
    }


type alias PublicRecordDto =
    { userName : String
    , bestScore : Int
    }


new : PublicRecordDto -> PublicRecord
new { userName, bestScore } =
    { userName = userName
    , bestScore = bestScore
    }


toStringUserName : Maybe PublicRecord -> String
toStringUserName maybeRecord =
    maybeRecord
        |> Maybe.map .userName
        |> Maybe.withDefault "---"


toStringBestScore : Maybe PublicRecord -> String
toStringBestScore maybeRecord =
    maybeRecord
        |> Maybe.map (.bestScore >> String.fromInt)
        |> Maybe.withDefault "---"
