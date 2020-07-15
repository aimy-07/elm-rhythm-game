module Page.Play.ResultSavingS exposing
    ( ResultSavingS
    , gotCurrentOwnRecord
    , gotCurrentPublicRecord
    , init
    , savedResult
    , savedUpdatedOwnRecord
    , savedUpdatedPublicRecord
    , startSaving
    , toResult
    )

import OwnRecord exposing (OwnRecordDto)
import Page.Play.Result as Result exposing (Result)
import PublicRecord exposing (PublicRecordDto)


type ResultSavingS
    = Init
    | ConnectingDB
        { result : Result
        , resultSavingS : SavingS
        , ownRecordSavingS : SavingS
        , publicRecordSavingS : SavingS
        }
    | Completed Result


type SavingS
    = Prepare
    | Saving
    | Saved


init : ResultSavingS
init =
    Init


toResult : ResultSavingS -> Maybe Result
toResult resultSavingS =
    case resultSavingS of
        Completed result ->
            Just result

        Init ->
            Nothing

        ConnectingDB _ ->
            Nothing


startSaving : Result -> ResultSavingS -> ( ResultSavingS, Cmd msg )
startSaving result resultSavingS =
    case resultSavingS of
        Init ->
            ( ConnectingDB
                { result = result
                , resultSavingS = Saving
                , ownRecordSavingS = Prepare
                , publicRecordSavingS = Prepare
                }
            , Cmd.batch
                [ Result.saveResult result
                , OwnRecord.getOwnRecord { uid = Result.toUid result, csvFileName = Result.toCsvFileName result }
                , PublicRecord.getPublicRecord <| Result.toCsvFileName result
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
                result =
                    savingS.result

                ownRecord =
                    OwnRecord.new ownRecordDto
            in
            ( ConnectingDB
                { savingS
                    | result = Result.updateIsBest (OwnRecord.toBestCombo ownRecord) (OwnRecord.toBestScore ownRecord) result
                    , ownRecordSavingS = Saving
                }
            , OwnRecord.saveOwnRecord
                { uid = OwnRecord.toUid ownRecord
                , csvFileName = OwnRecord.toCsvFileName ownRecord
                , combo = Result.toCombo result
                , bestCombo = OwnRecord.toBestCombo ownRecord
                , score = Result.toScore result
                , bestScore = OwnRecord.toBestScore ownRecord
                , playCount = OwnRecord.toPlayCount ownRecord
                }
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
                result =
                    savingS.result

                publicRecord =
                    PublicRecord.new publicRecordDto
            in
            ( ConnectingDB { savingS | publicRecordSavingS = Saving }
            , PublicRecord.savePublicRecord
                { uid = Result.toUid result
                , score = Result.toScore result
                , createdAt = Result.toCreatedAt result
                , csvFileName = PublicRecord.toCsvFileName publicRecord
                , bestRecords = PublicRecord.toBestRecords publicRecord
                }
            )

        Init ->
            ( resultSavingS, Cmd.none )

        Completed _ ->
            ( resultSavingS, Cmd.none )


savedResult : ResultSavingS -> ResultSavingS
savedResult resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            ConnectingDB { savingS | resultSavingS = Saved }
                |> toCompleted

        Init ->
            resultSavingS

        Completed _ ->
            resultSavingS


savedUpdatedOwnRecord : ResultSavingS -> ResultSavingS
savedUpdatedOwnRecord resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            ConnectingDB { savingS | ownRecordSavingS = Saved }
                |> toCompleted

        Init ->
            resultSavingS

        Completed _ ->
            resultSavingS


savedUpdatedPublicRecord : ResultSavingS -> ResultSavingS
savedUpdatedPublicRecord resultSavingS =
    case resultSavingS of
        ConnectingDB savingS ->
            ConnectingDB { savingS | publicRecordSavingS = Saved }
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

        ConnectingDB savingS ->
            if savingS.resultSavingS == Saved && savingS.ownRecordSavingS == Saved && savingS.publicRecordSavingS == Saved then
                Completed savingS.result

            else
                resultSavingS

        Completed _ ->
            resultSavingS
