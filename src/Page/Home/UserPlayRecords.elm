module Page.Home.UserPlayRecords exposing
    ( UserPlayRecords
    , findByCsvFileName
    , gotOwnRecord
    , init
    , initCmd
    , isLoaded
    )

import AllMusicData.MusicData.CsvFileName as CsvFileName exposing (CsvFileName)
import Constants exposing (allModeList, allMusicIdList)
import Dict exposing (Dict)
import OwnRecord exposing (OwnRecordDto)
import Page.Home.UserPlayRecords.UserPlayRecord as UserPlayRecord exposing (UserPlayRecord)
import Session.User.Uid exposing (Uid)


type UserPlayRecords
    = Loading UserPlayRecordDict
    | Loaded UserPlayRecordDict


type alias UserPlayRecordDict =
    Dict String UserPlayRecord


init : UserPlayRecords
init =
    Loading Dict.empty


initCmd : Uid -> Cmd msg
initCmd uid =
    allMusicIdList
        |> List.map (\musicId -> List.map (CsvFileName.new musicId) allModeList)
        |> List.concat
        |> List.map (\csvFileName -> OwnRecord.getOwnRecord { uid = uid, csvFileName = csvFileName })
        |> Cmd.batch


isLoaded : UserPlayRecords -> Bool
isLoaded userPlayRecords =
    case userPlayRecords of
        Loaded _ ->
            True

        Loading _ ->
            False


gotOwnRecord : OwnRecordDto -> UserPlayRecords -> UserPlayRecords
gotOwnRecord ownRecordDto userPlayRecords =
    case userPlayRecords of
        Loading userPlayRecordDict ->
            let
                nextUserPlayRecordDict =
                    Dict.insert ownRecordDto.csvFileName (UserPlayRecord.new ownRecordDto) userPlayRecordDict
            in
            if Dict.size nextUserPlayRecordDict == List.length allMusicIdList * List.length allModeList then
                Loaded nextUserPlayRecordDict

            else
                Loading nextUserPlayRecordDict

        Loaded _ ->
            userPlayRecords


findByCsvFileName : CsvFileName -> UserPlayRecords -> Maybe UserPlayRecord
findByCsvFileName csvFileName userPlayRecords =
    case userPlayRecords of
        Loading _ ->
            Nothing

        Loaded userPlayRecordDict ->
            Dict.get csvFileName userPlayRecordDict
