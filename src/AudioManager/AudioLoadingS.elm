port module AudioManager.AudioLoadingS exposing
    ( AudioLoadingS
    , init
    , isLoaded
    , loadAudioInitial
    , loadBGM
    , loadSE
    , loaded
    , loadedAudioInitial
    , loadedBGM
    , loadedSE
    , update
    , updateBGMLoadingProgress
    , updateInitial
    , updateSELoadingProgress
    )

import AudioManager.BGM as BGM
import AudioManager.SE as SE
import Dict exposing (Dict)


type AudioLoadingS
    = Loading BGMLoadingProgress SELoadingProgress
    | Loaded


type alias BGMLoadingProgress =
    LoadingProgress


type alias SELoadingProgress =
    LoadingProgress


type alias LoadingProgress =
    Dict String Bool


init : AudioLoadingS
init =
    Loading
        (BGM.bgmKeyList
            |> List.map (\bgmKey -> ( bgmKey, False ))
            |> Dict.fromList
        )
        (SE.seKeyList
            |> List.map (\seKey -> ( seKey, False ))
            |> Dict.fromList
        )


loaded : AudioLoadingS
loaded =
    Loaded


isLoaded : AudioLoadingS -> Bool
isLoaded audioLoadingS =
    case audioLoadingS of
        Loaded ->
            True

        Loading _ _ ->
            False


updateInitial : AudioLoadingS -> AudioLoadingS
updateInitial audioLoadingS =
    case audioLoadingS of
        Loading bgmProgress seProgress ->
            Loading (Dict.update "theRoadToHeaven" (Maybe.map (\_ -> True)) bgmProgress) seProgress

        Loaded ->
            audioLoadingS


update : Bool -> AudioLoadingS -> AudioLoadingS
update hasUserOperation audioLoadingS =
    case audioLoadingS of
        Loading bgmProgress seProgress ->
            if isCompletedProgress bgmProgress && isCompletedProgress seProgress && hasUserOperation then
                Loaded

            else
                audioLoadingS

        Loaded ->
            audioLoadingS


updateBGMLoadingProgress : String -> AudioLoadingS -> AudioLoadingS
updateBGMLoadingProgress bgmKey audioLoadingS =
    case audioLoadingS of
        Loading bgmProgress seProgress ->
            Loading (Dict.update bgmKey (Maybe.map (\_ -> True)) bgmProgress) seProgress

        Loaded ->
            audioLoadingS


updateSELoadingProgress : String -> AudioLoadingS -> AudioLoadingS
updateSELoadingProgress seKey audioLoadingS =
    case audioLoadingS of
        Loading bgmProgress seProgress ->
            Loading bgmProgress (Dict.update seKey (Maybe.map (\_ -> True)) seProgress)

        Loaded ->
            audioLoadingS


isCompletedProgress : LoadingProgress -> Bool
isCompletedProgress progress =
    Dict.values progress
        |> List.member False
        |> not


port loadAudioInitial : () -> Cmd msg


port loadedAudioInitial : (() -> msg) -> Sub msg


port loadBGM : () -> Cmd msg


port loadedBGM : (String -> msg) -> Sub msg


port loadSE : () -> Cmd msg


port loadedSE : (String -> msg) -> Sub msg
