port module PublicRecord exposing
    ( BestRecord
    , PublicRecord
    , PublicRecordDto
    , getPublicRecord
    , gotPublicRecord
    , new
    , savePublicRecord
    , savedPublicRecord
    , sortBestRecords
    , toBestRecords
    , toCsvFileName
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import Session.User.Uid exposing (Uid)


type PublicRecord
    = PublicRecord
        { csvFileName : CsvFileName
        , bestRecords : List BestRecord
        }


type alias BestRecord =
    { uid : Uid
    , score : Int
    , createdAt : CreatedAt
    }


type alias CreatedAt =
    Float


type alias PublicRecordDto =
    { csvFileName : String
    , bestRecords : Maybe (List BestRecordDto)
    }


type alias BestRecordDto =
    { uid : String
    , score : Int
    , createdAt : Float
    }


toCsvFileName : PublicRecord -> CsvFileName
toCsvFileName (PublicRecord { csvFileName }) =
    csvFileName


toBestRecords : PublicRecord -> List BestRecord
toBestRecords (PublicRecord { bestRecords }) =
    bestRecords


new : PublicRecordDto -> PublicRecord
new { csvFileName, bestRecords } =
    PublicRecord
        { csvFileName = csvFileName
        , bestRecords =
            bestRecords
                |> Maybe.map (List.map newBestRecord)
                |> Maybe.withDefault []
        }


newBestRecord : BestRecordDto -> BestRecord
newBestRecord { uid, score, createdAt } =
    { uid = uid
    , score = score
    , createdAt = createdAt
    }


{-| scoreでソートし、同scoreだった場合はcreatedAtが新しいものを上位に持ってくる
-}
sortBestRecords : List BestRecord -> List BestRecord
sortBestRecords bestRecords =
    let
        comparison a b =
            if Basics.compare a.score b.score == EQ then
                Basics.compare a.createdAt b.createdAt

            else
                Basics.compare a.score b.score
    in
    bestRecords
        |> List.sortWith comparison
        |> List.reverse


savePublicRecord :
    { uid : Uid
    , score : Int
    , createdAt : CreatedAt
    , csvFileName : CsvFileName
    , bestRecords : List BestRecord
    }
    -> Cmd msg
savePublicRecord { uid, score, createdAt, csvFileName, bestRecords } =
    if score > 0 then
        let
            newRecord =
                { uid = uid
                , score = score
                , createdAt = createdAt
                }

            nextBestRecords =
                (newRecord :: bestRecords)
                    |> sortBestRecords
                    |> List.take 3
        in
        savePublicRecord_
            { csvFileName = csvFileName
            , bestRecords = Just nextBestRecords
            }

    else
        Cmd.none


port getPublicRecord : String -> Cmd msg


port gotPublicRecord : (PublicRecordDto -> msg) -> Sub msg


port savePublicRecord_ : PublicRecordDto -> Cmd msg


port savedPublicRecord : (() -> msg) -> Sub msg
