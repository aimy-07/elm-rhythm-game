module Page.Home.RankingRecords exposing
    ( RankingData
    , RankingRecords
    , findByCsvFileName
    , gotPublicRecord
    , gotUsers
    , init
    , initCmd
    , isOwnRecord
    )

import AllMusicData.MusicData.CsvFileName as CsvFileName exposing (CsvFileName)
import Constants exposing (allModeList, allMusicIdList)
import Dict exposing (Dict)
import PublicRecord exposing (PublicRecord, PublicRecordDto)
import Session.User as User exposing (User, UserDto)
import Session.User.Uid exposing (Uid)
import Set


type RankingRecords
    = LoadingPublicRecord PublicRecordDict
    | LoadingUser PublicRecordDict
    | Loaded RankingDict


type alias PublicRecordDict =
    Dict String PublicRecord


type alias RankingDict =
    Dict String RankingData


type alias RankingData =
    { first : Maybe BestRecord
    , second : Maybe BestRecord
    , third : Maybe BestRecord
    }


type alias BestRecord =
    { user : User
    , score : Int
    }


init : RankingRecords
init =
    LoadingPublicRecord Dict.empty


initCmd : Cmd msg
initCmd =
    allMusicIdList
        |> List.map (\musicId -> List.map (CsvFileName.new musicId) allModeList)
        |> List.concat
        |> List.map PublicRecord.getPublicRecord
        |> Cmd.batch



-- List.map2 CsvFileName.new allMusicIdList allModeList
--     |> List.map PublicRecord.getPublicRecord
--     |> Cmd.batch


gotPublicRecord : PublicRecordDto -> RankingRecords -> ( RankingRecords, Cmd msg )
gotPublicRecord publicRecordDto rankingRecords =
    case rankingRecords of
        LoadingPublicRecord publicRecordDict ->
            let
                publicRecord =
                    PublicRecord.new publicRecordDto

                nextPublicRecordDict =
                    Dict.insert publicRecord.csvFileName publicRecord publicRecordDict
            in
            if Dict.size nextPublicRecordDict == List.length allMusicIdList * List.length allModeList then
                ( LoadingUser nextPublicRecordDict
                , nextPublicRecordDict
                    |> Dict.values
                    |> List.map .bestRecords
                    |> List.concat
                    |> List.map .uid
                    |> Set.fromList
                    -- 重複をなくすため一度Setに変換している
                    |> Set.toList
                    |> User.getUsers
                )

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
                        |> Dict.map (\_ publicRecord -> newRankingData users publicRecord)
            in
            Loaded rankingDataDict

        Loaded _ ->
            rankingRecords


newRankingData : List User -> PublicRecord -> RankingData
newRankingData users { bestRecords } =
    let
        first =
            bestRecords
                |> PublicRecord.sortBestRecords
                |> List.head
                |> convetUidToUser users

        second =
            bestRecords
                |> PublicRecord.sortBestRecords
                |> List.drop 1
                |> List.head
                |> convetUidToUser users

        third =
            bestRecords
                |> PublicRecord.sortBestRecords
                |> List.drop 2
                |> List.head
                |> convetUidToUser users
    in
    { first = first
    , second = second
    , third = third
    }


convetUidToUser : List User -> Maybe PublicRecord.BestRecord -> Maybe BestRecord
convetUidToUser users maybeBestRecord =
    maybeBestRecord
        |> Maybe.andThen
            (\{ uid, score } ->
                users
                    |> List.filter (.uid >> (==) uid)
                    |> List.head
                    |> Maybe.map (\user -> { user = user, score = score })
            )


findByCsvFileName : CsvFileName -> RankingRecords -> Maybe RankingData
findByCsvFileName csvFileName rankingRecords =
    case rankingRecords of
        LoadingPublicRecord _ ->
            Nothing

        LoadingUser _ ->
            Nothing

        Loaded rankingDict ->
            Dict.get csvFileName rankingDict


isOwnRecord : Maybe BestRecord -> Uid -> Bool
isOwnRecord maybeBestRecord uid =
    maybeBestRecord
        |> Maybe.map (.user >> .uid >> (==) uid)
        |> Maybe.withDefault False
