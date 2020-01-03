module Page.Play.ResultSavingS exposing
    ( ResultSavingS
    , gotCurrentOwnRecord
    , gotCurrentPublicRecord
    , init
    , savedRecord
    , savedUpdatedOwnRecord
    , savedUpdatedPublicRecord
    , startSaving
    , toResult
    )

import OwnRecord exposing (OwnRecord, OwnRecordDto)
import PublicRecord exposing (PublicRecord, PublicRecordDto)
import Record exposing (Record)


type ResultSavingS
    = Init
    | ConnectingDB
        { record : Record
        , recordSavingS : SavingS Record
        , ownRecordSavingS : SavingS OwnRecord
        , publicRecordSavingS : SavingS PublicRecord
        }
    | Completed
        { record : Record
        , isBestCombo : Bool
        , isBestScore : Bool
        }


type SavingS record
    = Prepare
    | Saving record
    | Saved record


init : ResultSavingS
init =
    Init


toResult : ResultSavingS -> Maybe { record : Record, isBestCombo : Bool, isBestScore : Bool }
toResult resultSavingS =
    case resultSavingS of
        Init ->
            Nothing

        ConnectingDB _ ->
            Nothing

        Completed result ->
            Just result


startSaving : Record -> ResultSavingS -> ( ResultSavingS, Cmd msg )
startSaving record resultSavingS =
    case resultSavingS of
        Init ->
            ( ConnectingDB
                { record = record
                , recordSavingS = Saving record
                , ownRecordSavingS = Prepare
                , publicRecordSavingS = Prepare
                }
            , Cmd.batch
                [ Record.saveRecord record
                , OwnRecord.getOwnRecord { uid = record.uid, csvFileName = record.csvFileName }
                , PublicRecord.getPublicRecord record.csvFileName
                ]
            )

        ConnectingDB _ ->
            ( resultSavingS, Cmd.none )

        Completed _ ->
            ( resultSavingS, Cmd.none )


gotCurrentOwnRecord : OwnRecordDto -> ResultSavingS -> ( ResultSavingS, Cmd msg )
gotCurrentOwnRecord ownRecordDto resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            let
                ownRecord =
                    OwnRecord.new ownRecordDto

                updatedOwnRecord =
                    OwnRecord.update savingS.record ownRecord
            in
            ( ConnectingDB { savingS | ownRecordSavingS = toSaving ownRecord savingS.ownRecordSavingS }
            , OwnRecord.saveOwnRecord updatedOwnRecord
            )

        Init ->
            ( resultSavingS, Cmd.none )

        Completed _ ->
            ( resultSavingS, Cmd.none )


gotCurrentPublicRecord : PublicRecordDto -> ResultSavingS -> ( ResultSavingS, Cmd msg )
gotCurrentPublicRecord publicRecordDto resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            let
                currentPublicRecord =
                    PublicRecord.new publicRecordDto

                updatedPublicRecord =
                    PublicRecord.update savingS.record currentPublicRecord
            in
            ( ConnectingDB { savingS | publicRecordSavingS = toSaving currentPublicRecord savingS.publicRecordSavingS }
            , PublicRecord.savePublicRecord updatedPublicRecord
            )

        Init ->
            ( resultSavingS, Cmd.none )

        Completed _ ->
            ( resultSavingS, Cmd.none )


savedRecord : ResultSavingS -> ResultSavingS
savedRecord resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            ConnectingDB { savingS | recordSavingS = toSaved savingS.recordSavingS }
                |> toCompleted

        Init ->
            resultSavingS

        Completed _ ->
            resultSavingS


savedUpdatedOwnRecord : ResultSavingS -> ResultSavingS
savedUpdatedOwnRecord resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            ConnectingDB { savingS | ownRecordSavingS = toSaved savingS.ownRecordSavingS }
                |> toCompleted

        Init ->
            resultSavingS

        Completed _ ->
            resultSavingS


savedUpdatedPublicRecord : ResultSavingS -> ResultSavingS
savedUpdatedPublicRecord resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            ConnectingDB { savingS | publicRecordSavingS = toSaved savingS.publicRecordSavingS }
                |> toCompleted

        Init ->
            resultSavingS

        Completed _ ->
            resultSavingS


toCompleted : ResultSavingS -> ResultSavingS
toCompleted resultSavingS =
    case resultSavingS of
        Init ->
            resultSavingS

        ConnectingDB { record, recordSavingS, ownRecordSavingS, publicRecordSavingS } ->
            case maybeSavedRecord ownRecordSavingS of
                Just pastOwnRecord ->
                    if isSaved recordSavingS && isSaved publicRecordSavingS then
                        Completed (newResult record pastOwnRecord)

                    else
                        resultSavingS

                Nothing ->
                    resultSavingS

        Completed _ ->
            resultSavingS


newResult : Record -> OwnRecord -> { record : Record, isBestCombo : Bool, isBestScore : Bool }
newResult record pastOwnRecord =
    let
        isBestCombo =
            pastOwnRecord.bestCombo
                |> Maybe.map (\bestCombo -> record.combo > bestCombo)
                |> Maybe.withDefault (record.combo > 0)

        isBestScore =
            pastOwnRecord.bestScore
                |> Maybe.map (\bestScore -> record.score > bestScore)
                |> Maybe.withDefault (record.score > 0)
    in
    { record = record
    , isBestCombo = isBestCombo
    , isBestScore = isBestScore
    }


toSaving : record -> SavingS record -> SavingS record
toSaving record savingS =
    case savingS of
        Prepare ->
            Saving record

        Saving _ ->
            savingS

        Saved _ ->
            savingS


toSaved : SavingS record -> SavingS record
toSaved savingS =
    case savingS of
        Prepare ->
            savingS

        Saving record ->
            Saved record

        Saved _ ->
            savingS


isSaved : SavingS record -> Bool
isSaved savingS =
    case savingS of
        Prepare ->
            False

        Saving _ ->
            False

        Saved _ ->
            True


maybeSavedRecord : SavingS record -> Maybe record
maybeSavedRecord savingS =
    case savingS of
        Prepare ->
            Nothing

        Saving _ ->
            Nothing

        Saved record ->
            Just record
