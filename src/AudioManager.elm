port module AudioManager exposing
    ( changeBgmVolume
    , getCurrentBGMTime
    , gotCurrentBGMTime
    , onEndBGM
    , pauseBGM
    , playBGM
    , playSE
    , stopBGM
    , unPauseBGM
    )

import AudioManager.BGM as BGM exposing (BGM)
import AudioManager.SE as SE exposing (SE)
import Constants exposing (bgmVolumeDefault, seVolumeDefault)
import UserSetting.Setting.Volume exposing (Volume)


playBGM : BGM -> Maybe Volume -> Cmd msg
playBGM bgm maybeVolume =
    let
        bgmVolume =
            maybeVolume
                |> Maybe.map identity
                |> Maybe.withDefault bgmVolumeDefault
    in
    BGM.unwrap bgm
        |> Maybe.map (\bgmKey -> playBGM_ { bgmKey = bgmKey, volume = bgmVolume })
        |> Maybe.withDefault Cmd.none


playSE : SE -> Maybe Volume -> Cmd msg
playSE se maybeVolume =
    let
        seVolume =
            maybeVolume
                |> Maybe.map identity
                |> Maybe.withDefault seVolumeDefault
    in
    playSE_ { seKey = SE.unwrap se, volume = seVolume }


pauseBGM : BGM -> Cmd msg
pauseBGM bgm =
    BGM.unwrap bgm
        |> Maybe.map pauseBGM_
        |> Maybe.withDefault Cmd.none


unPauseBGM : BGM -> Cmd msg
unPauseBGM bgm =
    BGM.unwrap bgm
        |> Maybe.map unPauseBGM_
        |> Maybe.withDefault Cmd.none


getCurrentBGMTime : BGM -> Cmd msg
getCurrentBGMTime bgm =
    BGM.unwrap bgm
        |> Maybe.map getCurrentBGMTime_
        |> Maybe.withDefault Cmd.none


port playBGM_ : { bgmKey : String, volume : Float } -> Cmd msg


port playSE_ : { seKey : String, volume : Float } -> Cmd msg


port pauseBGM_ : String -> Cmd msg


port unPauseBGM_ : String -> Cmd msg


port stopBGM : () -> Cmd msg


port getCurrentBGMTime_ : String -> Cmd msg


port gotCurrentBGMTime : (Float -> msg) -> Sub msg


port onEndBGM : (() -> msg) -> Sub msg


port changeBgmVolume : Float -> Cmd msg
