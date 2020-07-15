module Page.Home.RankingRecords exposing
    ( RankingRecords
    , findByCsvFileName
    , gotPublicRecord
    , gotUsers
    , init
    , initCmd
    , isLoaded
    )

import AllMusicData.MusicData.CsvFileName as CsvFileName exposing (CsvFileName)
import Constants exposing (allModeList, allMusicIdList)
import Dict exposing (Dict)
import Page.Home.RankingRecords.RankingRecord as RankingRecord exposing (RankingRecord)
import PublicRecord exposing (PublicRecord, PublicRecordDto)
import Session.User as User exposing (UserDto)
import Set


type RankingRecords
    = LoadingPublicRecord PublicRecordDict
    | LoadingUser PublicRecordDict
    | Loaded RankingRecordDict


type alias PublicRecordDict =
    Dict String PublicRecord


type alias RankingRecordDict =
    Dict String RankingRecord


init : RankingRecords
init =
    LoadingPublicRecord Dict.empty


initCmd : Cmd msg
initCmd =
    allMusicIdList
        |> List.map (\musicId -> List.map (CsvFileName.create musicId) allModeList)
        |> List.concat
        |> List.map PublicRecord.getPublicRecord
        |> Cmd.batch


isLoaded : RankingRecords -> Bool
isLoaded rankingRecords =
    case rankingRecords of
        Loaded _ ->
            True

        LoadingPublicRecord _ ->
            False

        LoadingUser _ ->
            False


gotPublicRecord : PublicRecordDto -> RankingRecords -> ( RankingRecords, Cmd msg )
gotPublicRecord publicRecordDto rankingRecords =
    case rankingRecords of
        LoadingPublicRecord publicRecordDict ->
            let
                publicRecord =
                    PublicRecord.new publicRecordDto

                nextPublicRecordDict =
                    Dict.insert (PublicRecord.toCsvFileName publicRecord) publicRecord publicRecordDict
            in
            if Dict.size nextPublicRecordDict == List.length allMusicIdList * List.length allModeList then
                let
                    rankingUsers =
                        nextPublicRecordDict
                            |> Dict.values
                            |> List.map PublicRecord.toBestRecords
                            |> List.concat
                            |> List.map .uid
                            |> Set.fromList
                            -- 重複をなくすため一度Setに変換している
                            |> Set.toList
                in
                ( LoadingUser nextPublicRecordDict, User.getUsers rankingUsers )

            else
                ( LoadingPublicRecord nextPublicRecordDict, Cmd.none )

        LoadingUser _ ->
            ( rankingRecords, Cmd.none )

        Loaded _ ->
            ( rankingRecords, Cmd.none )


gotUsers : List UserDto -> RankingRecords -> RankingRecords
gotUsers userDtos rankingRecords =
    case rankingRecords of
        LoadingPublicRecord _ ->
            rankingRecords

        LoadingUser publicRecordDict ->
            let
                users =
                    List.map User.new userDtos

                rankingDataDict =
                    publicRecordDict
                        |> Dict.map (\_ publicRecord -> RankingRecord.new users publicRecord)
            in
            Loaded rankingDataDict

        Loaded _ ->
            rankingRecords


findByCsvFileName : CsvFileName -> RankingRecords -> Maybe RankingRecord
findByCsvFileName csvFileName rankingRecords =
    case rankingRecords of
        LoadingPublicRecord _ ->
            Nothing

        LoadingUser _ ->
            Nothing

        Loaded rankingDict ->
            Dict.get csvFileName rankingDict
