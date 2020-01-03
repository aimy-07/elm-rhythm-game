module Page.Home.UserPlayRecords exposing
    ( UserPlayRecordData
    , UserPlayRecords
    , findByCsvFileName
    , gotOwnRecord
    , init
    , initCmd
    )

import AllMusicData.MusicData.CsvFileName as CsvFileName exposing (CsvFileName)
import Constants exposing (allModeList, allMusicIdList)
import Dict exposing (Dict)
import OwnRecord exposing (OwnRecord, OwnRecordDto)
import Session.User.Uid exposing (Uid)


type UserPlayRecords
    = LoadingOwnRecord UserPlayRecordDict
    | Loaded UserPlayRecordDict


type alias UserPlayRecordDict =
    Dict String UserPlayRecordData


type alias UserPlayRecordData =
    { bestCombo : Maybe Int
    , bestScore : Maybe Int
    , playCount : Int
    }


init : UserPlayRecords
init =
    LoadingOwnRecord Dict.empty


initCmd : Uid -> Cmd msg
initCmd uid =
    allMusicIdList
        |> List.map (\musicId -> List.map (CsvFileName.new musicId) allModeList)
        |> List.concat
        |> List.map (\csvFileName -> OwnRecord.getOwnRecord { uid = uid, csvFileName = csvFileName })
        |> Cmd.batch



-- List.map2 CsvFileName.new allMusicIdList allModeList
--     |> List.map (\csvFileName -> OwnRecord.getOwnRecord { uid = uid, csvFileName = csvFileName })
--     |> Cmd.batch


gotOwnRecord : OwnRecordDto -> UserPlayRecords -> UserPlayRecords
gotOwnRecord ownRecordDto userPlayRecords =
    case userPlayRecords of
        LoadingOwnRecord userPlayRecordDict ->
            let
                newOwnRecord =
                    OwnRecord.new ownRecordDto

                nextUserPlayRecordDict =
                    Dict.insert newOwnRecord.csvFileName (newUserPlayRecordData newOwnRecord) userPlayRecordDict
            in
            if Dict.size nextUserPlayRecordDict == List.length allMusicIdList * List.length allModeList then
                Loaded nextUserPlayRecordDict

            else
                LoadingOwnRecord nextUserPlayRecordDict

        Loaded _ ->
            userPlayRecords


newUserPlayRecordData : OwnRecord -> UserPlayRecordData
newUserPlayRecordData ownRecord =
    { bestCombo = ownRecord.bestCombo
    , bestScore = ownRecord.bestScore
    , playCount = ownRecord.playCount
    }


findByCsvFileName : CsvFileName -> UserPlayRecords -> Maybe UserPlayRecordData
findByCsvFileName csvFileName userPlayRecords =
    case userPlayRecords of
        LoadingOwnRecord _ ->
            Nothing

        Loaded userPlayRecordDict ->
            Dict.get csvFileName userPlayRecordDict
