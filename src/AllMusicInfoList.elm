port module AllMusicInfoList exposing
    ( AllMusicInfoList
    , getAllMusicInfoList
    , gotAllMusicInfoList
    , init
    , isLoaded
    , setAudioInfo
    , setMusicInfo
    , toAudioInfoFindByMusicId
    , toAudioInfoList
    , toMusicInfoFindByCsvFileName
    , toMusicInfoList
    , toMusicInfoListFilterByMode
    )

import AudioManager.AudioInfo as AudioInfo exposing (AudioInfo, AudioInfoDto)
import MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import Utils exposing (cmdIf)


type AllMusicInfoList
    = Loaded (List MusicInfo) (List AudioInfo)
    | Loading (List MusicInfo) (List AudioInfo)
    | NotLoaded


init : AllMusicInfoList
init =
    NotLoaded


setMusicInfo : List MusicInfoDto -> AllMusicInfoList -> AllMusicInfoList
setMusicInfo musicInfoDtos allMusicInfoList =
    case allMusicInfoList of
        NotLoaded ->
            Loading (List.map MusicInfo.new musicInfoDtos) []

        Loading _ _ ->
            allMusicInfoList

        Loaded _ _ ->
            allMusicInfoList


setAudioInfo : AudioInfoDto -> AllMusicInfoList -> AllMusicInfoList
setAudioInfo audioInfoDto allMusicInfoList =
    case allMusicInfoList of
        Loading musicInfoList audioInfoList ->
            let
                nextAudioInfoList =
                    AudioInfo.new audioInfoDto :: audioInfoList
            in
            if List.length nextAudioInfoList >= List.length musicInfoList // 3 then
                Loaded musicInfoList nextAudioInfoList

            else
                Loading musicInfoList nextAudioInfoList

        Loaded _ _ ->
            allMusicInfoList

        NotLoaded ->
            allMusicInfoList


isLoaded : AllMusicInfoList -> Bool
isLoaded allMusicInfoList =
    case allMusicInfoList of
        Loaded _ _ ->
            True

        Loading _ _ ->
            False

        NotLoaded ->
            False


toMusicInfoList : AllMusicInfoList -> List MusicInfo
toMusicInfoList allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList _ ->
            musicInfoList

        Loading _ _ ->
            []

        NotLoaded ->
            []


toMusicInfoListFilterByMode : Mode -> AllMusicInfoList -> List MusicInfo
toMusicInfoListFilterByMode mode allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList _ ->
            musicInfoList
                |> List.filter (.mode >> (==) mode)

        Loading _ _ ->
            []

        NotLoaded ->
            []


toMusicInfoFindByCsvFileName : CsvFileName -> AllMusicInfoList -> Maybe MusicInfo
toMusicInfoFindByCsvFileName csvFileName allMusicInfoList =
    case allMusicInfoList of
        Loaded musicInfoList _ ->
            musicInfoList
                |> List.filter (.csvFileName >> (==) csvFileName)
                |> List.head

        Loading _ _ ->
            Nothing

        NotLoaded ->
            Nothing


toAudioInfoList : AllMusicInfoList -> List AudioInfo
toAudioInfoList allMusicInfoList =
    case allMusicInfoList of
        Loaded _ audioInfoList ->
            audioInfoList

        Loading _ _ ->
            []

        NotLoaded ->
            []


toAudioInfoFindByMusicId : MusicId -> AllMusicInfoList -> Maybe AudioInfo
toAudioInfoFindByMusicId musicId allMusicInfoList =
    case allMusicInfoList of
        Loaded _ audioInfoList ->
            audioInfoList
                |> List.filter (\audioInfo -> audioInfo.audioFileName == musicId ++ "_sample")
                |> List.head

        Loading _ _ ->
            Nothing

        NotLoaded ->
            Nothing


getAllMusicInfoList : AllMusicInfoList -> Cmd msg
getAllMusicInfoList allMusicInfoList =
    getAllMusicInfoList_ ()
        |> cmdIf (not <| isLoaded allMusicInfoList)


port getAllMusicInfoList_ : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg
