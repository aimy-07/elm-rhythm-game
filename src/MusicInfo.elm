module MusicInfo exposing
    ( MusicInfo
    , MusicInfoDto
    , empty
    , new
    , toStringTime
    )

import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Level exposing (Level)
import MusicInfo.Mode as Mode exposing (Mode)


type alias MusicInfo =
    { csvFileName : CsvFileName
    , musicName : String
    , composer : String
    , mode : Mode
    , level : Level
    , fullTime : Float
    , bpm : Int
    , maxCombo : Int
    , maxScore : Int
    }


type alias MusicInfoDto =
    { csvFileName : String
    , musicName : String
    , composer : String
    , mode : String
    , level : Int
    , fullTime : Float
    , bpm : Int
    , maxCombo : Int
    , maxScore : Int
    }


new : MusicInfoDto -> MusicInfo
new musicInfoDto =
    { csvFileName = musicInfoDto.csvFileName
    , musicName = musicInfoDto.musicName
    , composer = musicInfoDto.composer
    , mode = Mode.new musicInfoDto.mode
    , level = musicInfoDto.level
    , fullTime = musicInfoDto.fullTime
    , bpm = musicInfoDto.bpm
    , maxCombo = musicInfoDto.maxCombo
    , maxScore = musicInfoDto.maxScore
    }


empty : MusicInfo
empty =
    { csvFileName = ""
    , musicName = ""
    , composer = ""
    , mode = Mode.new ""
    , level = 0
    , fullTime = 0
    , bpm = 0
    , maxCombo = 0
    , maxScore = 0
    }


toStringTime : Float -> String
toStringTime time =
    let
        sec =
            modBy 60 (Basics.round time)

        min =
            Basics.round time // 60
    in
    String.fromInt min ++ "分" ++ String.fromInt sec ++ "秒"
