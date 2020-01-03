module Page.Play.PlayingS exposing
    ( PlayingS
    , finish
    , finishedCountdown
    , init
    , isCountdown
    , isFinish
    , isPause
    , isPlaying
    , isReady
    , pressSpaceKey
    )

import AudioManager
import AudioManager.BGM exposing (BGM)
import Process
import Task
import UserSetting.Setting.Volume exposing (Volume)


type PlayingS
    = Ready
    | StartCountdown
    | Playing
    | Pause
    | PauseCountdown
    | Finish


init : PlayingS
init =
    Ready


finish : PlayingS
finish =
    Finish


isReady : PlayingS -> Bool
isReady playingS =
    playingS == Ready


isPlaying : PlayingS -> Bool
isPlaying playingS =
    playingS == Playing


isPause : PlayingS -> Bool
isPause playingS =
    playingS == Pause


isCountdown : PlayingS -> Bool
isCountdown playingS =
    playingS == StartCountdown || playingS == PauseCountdown


isFinish : PlayingS -> Bool
isFinish playingS =
    playingS == Finish


pressSpaceKey : BGM -> msg -> PlayingS -> ( PlayingS, Cmd msg )
pressSpaceKey bgm msg playingS =
    case playingS of
        Ready ->
            ( StartCountdown, Process.sleep 1500 |> Task.perform (\_ -> msg) )

        Playing ->
            ( Pause, AudioManager.pauseBGM bgm )

        Pause ->
            ( PauseCountdown, Process.sleep 1500 |> Task.perform (\_ -> msg) )

        _ ->
            ( playingS, Cmd.none )


finishedCountdown : BGM -> Maybe Volume -> PlayingS -> ( PlayingS, Cmd msg )
finishedCountdown bgm bgmVolume playingS =
    case playingS of
        StartCountdown ->
            ( Playing, AudioManager.playBGM bgm bgmVolume )

        PauseCountdown ->
            ( Playing, AudioManager.unPauseBGM bgm )

        _ ->
            ( playingS, Cmd.none )
