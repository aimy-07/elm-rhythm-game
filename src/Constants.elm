module Constants exposing
    ( allKeyList
    , allModeList
    , allMusicIdList
    , bgmVolumeDefault
    , currentModeDefault
    , currentMusicIdDefault
    , goodRange
    , goodScore
    , longTimeDuration
    , longTimeOffset
    , niceRange
    , niceScore
    , notesSpeedDefault
    , perfectRange
    , perfectScore
    , seVolumeDefault
    , tweetText
    )

import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)
import Page.Play.Key as Key exposing (Key)
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


allKeyList : List Key
allKeyList =
    [ Key.S, Key.D, Key.F, Key.J, Key.K, Key.L ]


longTimeDuration : Float
longTimeDuration =
    200


longTimeOffset : Float
longTimeOffset =
    30


perfectRange : Float
perfectRange =
    60


niceRange : Float
niceRange =
    100


goodRange : Float
goodRange =
    150


perfectScore : Int
perfectScore =
    5000


niceScore : Int
niceScore =
    2500


goodScore : Int
goodScore =
    1000


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
