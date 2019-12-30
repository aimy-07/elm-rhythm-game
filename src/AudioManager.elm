port module AudioManager exposing
    ( getAudioInfo
    , gotAudioInfo
    , pauseBGM
    , playBGM
    , playSE
    , unPauseBGM
    )

import AudioManager.AudioInfo exposing (AudioInfoDto)
import AudioManager.AudioUrl exposing (AudioUrl)
import AudioManager.SE as SE exposing (SE)
import Constants exposing (bgmVolumeDefault, seVolumeDefault)
import Setting.Volume exposing (Volume)


playBGM : Maybe AudioUrl -> Maybe Volume -> Bool -> Cmd msg
playBGM maybeAudioUrl maybeVolume isLoop =
    let
        bgmVolume =
            maybeVolume
                |> Maybe.map identity
                |> Maybe.withDefault bgmVolumeDefault
    in
    case maybeAudioUrl of
        Just audioUrl ->
            playBGM_ { audioUrl = audioUrl, volume = bgmVolume, isLoop = isLoop }

        Nothing ->
            Cmd.none


playSE : SE -> Maybe Volume -> Cmd msg
playSE se maybeVolume =
    let
        seVolume =
            maybeVolume
                |> Maybe.map identity
                |> Maybe.withDefault seVolumeDefault
    in
    playSE_ { audioUrl = SE.toAudioUrl se, volume = seVolume }


port getAudioInfo : String -> Cmd msg


port gotAudioInfo : (AudioInfoDto -> msg) -> Sub msg


port playBGM_ : { audioUrl : String, volume : Float, isLoop : Bool } -> Cmd msg


port pauseBGM : () -> Cmd msg


port unPauseBGM : () -> Cmd msg


port playSE_ : { audioUrl : String, volume : Float } -> Cmd msg
