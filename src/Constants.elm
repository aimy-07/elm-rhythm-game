module Constants exposing
    ( allKeyStr
    , allMode
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
    , notesSpeedLevel
    , perfectRange
    , perfectScore
    )

import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import Page.Play.KeyStr exposing (KeyStr)
import UserSetting.NotesSpeed exposing (NotesSpeed)


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
    50


greatRange : Float
greatRange =
    90


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


notesSpeedLevel : List NotesSpeed
notesSpeedLevel =
    [ 0.2, 0.3, 0.4, 0.5, 0.6 ]


notesSpeedDefault : NotesSpeed
notesSpeedDefault =
    0.4
