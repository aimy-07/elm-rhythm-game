module Constants exposing
    ( allKeyStrList
    , allModeList
    , allMusicIdList
    , bgmVolumeDefault
    , currentModeDefault
    , currentMusicIdDefault
    , goodRange
    , goodScore
    , greatRange
    , greatScore
    , longTimeDuration
    , longTimeOffset
    , lostScore
    , notesSpeedDefault
    , perfectRange
    , perfectScore
    , seVolumeDefault
    , tweetText
    )

import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)
import Page.Play.KeyStr exposing (KeyStr)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import UserSetting.Setting.Volume exposing (Volume)


allMusicIdList : List String
allMusicIdList =
    [ "sampleSound"
    , "sampleSoundShort"
    , "whiteGlow"
    ]


allModeList : List Mode
allModeList =
    [ Mode.Normal, Mode.Hard, Mode.Master ]


allKeyStrList : List KeyStr
allKeyStrList =
    [ "S", "D", "F", "J", "K", "L" ]


longTimeDuration : Float
longTimeDuration =
    200


longTimeOffset : Float
longTimeOffset =
    30


perfectRange : Float
perfectRange =
    70


greatRange : Float
greatRange =
    100


goodRange : Float
goodRange =
    130


perfectScore : Int
perfectScore =
    2000


greatScore : Int
greatScore =
    1500


goodScore : Int
goodScore =
    1000


lostScore : Int
lostScore =
    200


currentMusicIdDefault : MusicId
currentMusicIdDefault =
    "sampleSound"


currentModeDefault : Mode
currentModeDefault =
    Mode.Normal


notesSpeedDefault : NotesSpeed
notesSpeedDefault =
    0.5


bgmVolumeDefault : Volume
bgmVolumeDefault =
    0.5


seVolumeDefault : Volume
seVolumeDefault =
    0.5


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
