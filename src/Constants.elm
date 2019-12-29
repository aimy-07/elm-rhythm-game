module Constants exposing
    ( allKeyStr
    , allMode
    , bgmVolumeDefault
    , currentModeDefault
    , currentMusicIdDefault
    , goodRange
    , goodScore
    , greatRange
    , greatScore
    , longScore
    , longTimeDuration
    , longTimeOffset
    , missRange
    , notesSpeedDefault
    , perfectRange
    , perfectScore
    , seVolumeDefault
    , tweetText
    )

import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import Page.Play.KeyStr exposing (KeyStr)
import Setting.NotesSpeed exposing (NotesSpeed)
import Setting.Volume exposing (Volume)


allKeyStr : List KeyStr
allKeyStr =
    [ "S", "D", "F", "J", "K", "L" ]


longTimeOffset : Float
longTimeOffset =
    150


longTimeDuration : Float
longTimeDuration =
    200


perfectRange : Float
perfectRange =
    70


greatRange : Float
greatRange =
    100


goodRange : Float
goodRange =
    130


missRange : Float
missRange =
    200


perfectScore : Int
perfectScore =
    2000


greatScore : Int
greatScore =
    1500


goodScore : Int
goodScore =
    1000


longScore : Int
longScore =
    100


currentMusicIdDefault : MusicId
currentMusicIdDefault =
    "sample_sound"


currentModeDefault : Mode
currentModeDefault =
    Mode.normal


allMode : List Mode
allMode =
    [ Mode.normal, Mode.hard, Mode.master ]


notesSpeedDefault : NotesSpeed
notesSpeedDefault =
    0.4


bgmVolumeDefault : Volume
bgmVolumeDefault =
    0.7


seVolumeDefault : Volume
seVolumeDefault =
    0.7


tweetText : String -> Mode -> Int -> Int -> String
tweetText musicName mode score combo =
    let
        text1 =
            "【Elmで開発中のリズムゲームをプレイ中！】"

        text2 =
            musicName ++ "の" ++ Mode.toString mode ++ "モードで"

        text3 =
            "スコア" ++ String.fromInt score ++ "、コンボ" ++ String.fromInt combo ++ "を記録しました！"

        text4 =
            "※ テストです"

        url =
            ""

        hashtag =
            ""
    in
    text1 ++ text2 ++ text3 ++ text4 ++ url ++ hashtag
