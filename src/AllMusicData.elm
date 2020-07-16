module AllMusicData exposing
    ( AllMusicData
    , filterByMode
    , findByCsvFileName
    , init
    , isLoaded
    , isLoadedCsv
    , isLoadedJson
    , loadMusicDataByCsvCmds
    , loadMusicDataByJsonCmds
    , updateFromCsv
    , updateFromJson
    )

import AllMusicData.MusicData as MusicData exposing (MusicData, MusicDataDto)
import AllMusicData.MusicData.Csv exposing (CsvDto)
import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import AllMusicData.MusicData.Mode exposing (Mode)
import Constants exposing (allModeList, allMusicIdList)
import Dict exposing (Dict)


type AllMusicData
    = LoadingJson MusicDataDict
    | LoadingCsv MusicDataDict
    | Loaded MusicDataDict


type alias MusicDataDict =
    Dict String MusicData


init : AllMusicData
init =
    LoadingJson Dict.empty


isLoaded : AllMusicData -> Bool
isLoaded allMusicData =
    case allMusicData of
        LoadingJson _ ->
            False

        LoadingCsv _ ->
            False

        Loaded _ ->
            True


isLoadedJson : AllMusicData -> Bool
isLoadedJson allMusicData =
    Dict.size (toMusicDataDict allMusicData) == List.length allMusicIdList * List.length allModeList


isLoadedCsv : AllMusicData -> Bool
isLoadedCsv allMusicData =
    toMusicDataDict allMusicData
        |> Dict.values
        |> List.sortBy .order
        |> List.map (\{ maxCombo, maxScore } -> maxCombo == 0 || maxScore == 0)
        |> List.member True
        |> not


updateFromJson : MusicDataDto -> AllMusicData -> AllMusicData
updateFromJson jsonDto allMusicData =
    case allMusicData of
        LoadingJson musicDataDict ->
            let
                updatedMusicDataDict =
                    allModeList
                        |> List.map (MusicData.newFromJson jsonDto)
                        |> List.map (\musicData -> ( musicData.csvFileName, musicData ))
                        |> Dict.fromList
                        |> Dict.union musicDataDict

                updatedAllMusicData =
                    LoadingJson updatedMusicDataDict
            in
            if isLoadedJson updatedAllMusicData then
                LoadingCsv updatedMusicDataDict

            else
                updatedAllMusicData

        LoadingCsv _ ->
            allMusicData

        Loaded _ ->
            allMusicData


updateFromCsv : CsvDto -> AllMusicData -> AllMusicData
updateFromCsv csvDto allMusicData =
    case allMusicData of
        LoadingJson _ ->
            allMusicData

        LoadingCsv musicDataDict ->
            let
                updatedMusicDataDict =
                    Dict.update csvDto.csvFileName (MusicData.updateFromCsv csvDto.csvData) musicDataDict

                updatedAllMusicData =
                    LoadingCsv updatedMusicDataDict
            in
            if isLoadedCsv updatedAllMusicData then
                Loaded updatedMusicDataDict

            else
                updatedAllMusicData

        Loaded _ ->
            allMusicData


findByCsvFileName : CsvFileName -> AllMusicData -> Maybe MusicData
findByCsvFileName csvFileName allMusicData =
    if isLoaded allMusicData then
        allMusicData
            |> toMusicDataDict
            |> Dict.get csvFileName

    else
        Nothing


filterByMode : Mode -> AllMusicData -> List MusicData
filterByMode mode allMusicData =
    if isLoaded allMusicData then
        allMusicData
            |> toMusicDataDict
            |> Dict.values
            |> List.sortBy .order
            |> List.filter (.mode >> (==) mode)

    else
        []


toMusicDataDict : AllMusicData -> MusicDataDict
toMusicDataDict allMusicData =
    case allMusicData of
        LoadingJson musicDataDict ->
            musicDataDict

        LoadingCsv musicDataDict ->
            musicDataDict

        Loaded musicDataDict ->
            musicDataDict


loadMusicDataByJsonCmds : Cmd msg
loadMusicDataByJsonCmds =
    allMusicIdList
        |> List.map MusicData.loadMusicDataByJson
        |> Cmd.batch


loadMusicDataByCsvCmds : AllMusicData -> Cmd msg
loadMusicDataByCsvCmds allMusicData =
    toMusicDataDict allMusicData
        |> Dict.values
        |> List.sortBy .order
        |> List.map (.csvFileName >> MusicData.loadMusicDataByCsv)
        |> Cmd.batch
