port module OwnRecord exposing
    ( OwnRecord
    , OwnRecordDto
    , getOwnRecord
    , gotOwnRecord
    , new
    , saveOwnRecord
    , savedOwnRecord
    , update
    )

import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import Record exposing (Record)
import Session.User.Uid exposing (Uid)


type alias OwnRecord =
    { uid : Uid
    , csvFileName : CsvFileName
    , bestCombo : Maybe Int
    , bestScore : Maybe Int
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


new : OwnRecordDto -> OwnRecord
new { uid, csvFileName, playRecord } =
    { uid = uid
    , csvFileName = csvFileName
    , bestCombo =
        playRecord
            |> Maybe.map .bestCombo
    , bestScore =
        playRecord
            |> Maybe.map .bestScore
    , playCount =
        playRecord
            |> Maybe.map .playCount
            |> Maybe.withDefault 0
    }


{-| プレイが終了した時に、リザルトを元に個人のプレイデータを更新する
-}
update : Record -> OwnRecord -> OwnRecord
update record pastOwnRecord =
    case ( pastOwnRecord.bestCombo, pastOwnRecord.bestScore ) of
        ( Just bestCombo, Just bestScore ) ->
            { pastOwnRecord
                | bestCombo = Just <| Basics.max record.combo bestCombo
                , bestScore = Just <| Basics.max record.score bestScore
                , playCount = pastOwnRecord.playCount + 1
            }

        _ ->
            { pastOwnRecord
                | bestCombo = Just record.combo
                , bestScore = Just record.score
                , playCount = 1
            }


saveOwnRecord : OwnRecord -> Cmd msg
saveOwnRecord ownRecord =
    let
        bestCombo =
            ownRecord.bestCombo
                |> Maybe.withDefault 0

        bestScore =
            ownRecord.bestScore
                |> Maybe.withDefault 0
    in
    saveOwnRecord_
        { uid = ownRecord.uid
        , csvFileName = ownRecord.csvFileName
        , playRecord =
            Just
                { bestCombo = bestCombo
                , bestScore = bestScore
                , playCount = ownRecord.playCount
                }
        }


port getOwnRecord : { uid : String, csvFileName : String } -> Cmd msg


port gotOwnRecord : (OwnRecordDto -> msg) -> Sub msg


port saveOwnRecord_ : OwnRecordDto -> Cmd msg


port savedOwnRecord : (() -> msg) -> Sub msg
