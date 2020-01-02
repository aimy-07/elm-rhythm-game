module Page.Play.PlayStatus exposing
    ( PlayStatus
    , finish
    , finishedCountdown
    , init
    , isCountdown
    , isFinish
    , isPause
    , isPlaying
    , isPreFinish
    , isReady
    , preFinish
    , pressSpaceKey
    )

import AudioManager
import AudioManager.BGM exposing (BGM)
import Process
import Task
import UserSetting.Setting.Volume exposing (Volume)


type PlayStatus
    = Ready
    | StartCountdown
    | Playing
    | Pause
    | PauseCountdown
    | PreFinish
    | Finish


init : PlayStatus
init =
    Ready


preFinish : PlayStatus
preFinish =
    PreFinish


finish : PlayStatus
finish =
    Finish


isReady : PlayStatus -> Bool
isReady playStatus =
    playStatus == Ready


isPlaying : PlayStatus -> Bool
isPlaying playStatus =
    playStatus == Playing


isPause : PlayStatus -> Bool
isPause playStatus =
    playStatus == Pause


isCountdown : PlayStatus -> Bool
isCountdown playStatus =
    playStatus == StartCountdown || playStatus == PauseCountdown


isPreFinish : PlayStatus -> Bool
isPreFinish playStatus =
    playStatus == PreFinish


isFinish : PlayStatus -> Bool
isFinish playStatus =
    playStatus == Finish


pressSpaceKey : BGM -> msg -> PlayStatus -> ( PlayStatus, Cmd msg )
pressSpaceKey bgm msg playStatus =
    case playStatus of
        Ready ->
            ( StartCountdown, Process.sleep 1500 |> Task.perform (\_ -> msg) )

        Playing ->
            ( Pause, AudioManager.pauseBGM bgm )

        Pause ->
            ( PauseCountdown, Process.sleep 1500 |> Task.perform (\_ -> msg) )

        _ ->
            ( playStatus, Cmd.none )


finishedCountdown : BGM -> Maybe Volume -> PlayStatus -> ( PlayStatus, Cmd msg )
finishedCountdown bgm bgmVolume playStatus =
    case playStatus of
        StartCountdown ->
            ( Playing, AudioManager.playBGM bgm bgmVolume )

        PauseCountdown ->
            ( Playing, AudioManager.unPauseBGM bgm )

        _ ->
            ( playStatus, Cmd.none )
