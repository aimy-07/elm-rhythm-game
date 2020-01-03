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
    , update
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import CreatedAt exposing (CreatedAt)
import Record exposing (Record)
import Session.User.Uid exposing (Uid)


type alias PublicRecord =
    { csvFileName : CsvFileName
    , bestRecords : List BestRecord
    }


type alias BestRecord =
    { uid : Uid
    , score : Int
    , createdAt : CreatedAt
    }


type alias PublicRecordDto =
    { csvFileName : String
    , bestRecords : Maybe (List BestRecordDto)
    }


type alias BestRecordDto =
    { uid : String
    , score : Int
    , createdAt : Float
    }


new : PublicRecordDto -> PublicRecord
new { csvFileName, bestRecords } =
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


{-| プレイが終了した時に、リザルトを元にランキングデータを更新する
-}
update : Record -> PublicRecord -> PublicRecord
update record currentPublicRecord =
    let
        newRecord =
            { uid = record.uid
            , score = record.score
            , createdAt = record.createdAt
            }
    in
    if record.score > 0 then
        let
            nextBestRecords =
                (newRecord :: currentPublicRecord.bestRecords)
                    |> sortBestRecords
                    |> List.take 3
        in
        { currentPublicRecord | bestRecords = nextBestRecords }

    else
        currentPublicRecord


{-| scoreでソートし、同scoreだった場合はcreatedAtが新しいものを上位に持ってくる
-}
sortBestRecords : List BestRecord -> List BestRecord
sortBestRecords bestRecords =
    bestRecords
        |> List.sortWith comparison
        |> List.reverse


comparison : BestRecord -> BestRecord -> Order
comparison a b =
    if Basics.compare a.score b.score == EQ then
        Basics.compare a.createdAt b.createdAt

    else
        Basics.compare a.score b.score


savePublicRecord : PublicRecord -> Cmd msg
savePublicRecord publicRecord =
    savePublicRecord_
        { csvFileName = publicRecord.csvFileName
        , bestRecords =
            if List.isEmpty publicRecord.bestRecords then
                Nothing

            else
                Just publicRecord.bestRecords
        }


port getPublicRecord : String -> Cmd msg


port gotPublicRecord : (PublicRecordDto -> msg) -> Sub msg


port savePublicRecord_ : PublicRecordDto -> Cmd msg


port savedPublicRecord : (() -> msg) -> Sub msg
