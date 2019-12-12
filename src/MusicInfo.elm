module MusicInfo exposing
    ( MusicInfo
    , MusicInfoDto
    , empty
    , new
    , toBpm
    , toComposer
    , toCsvFileName
    , toFullTime
    , toLevel
    , toMaxCombo
    , toMaxScore
    , toMode
    , toMusicName
    , toStringLevel
    , toStringTime
    )

import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Mode as Mode exposing (Mode)


type MusicInfo
    = MusicInfo
        { csvFileName : CsvFileName
        , musicName : String
        , composer : String
        , mode : Mode
        , level : Int
        , bpm : Int
        , maxCombo : Int
        , maxScore : Int
        , fullTime : Float
        }


type alias MusicInfoDto =
    { csvFileName : CsvFileName
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
    MusicInfo
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
    MusicInfo
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


toCsvFileName : MusicInfo -> CsvFileName
toCsvFileName (MusicInfo { csvFileName }) =
    csvFileName


toMusicName : MusicInfo -> String
toMusicName (MusicInfo { musicName }) =
    musicName


toComposer : MusicInfo -> String
toComposer (MusicInfo { composer }) =
    composer


toMode : MusicInfo -> Mode
toMode (MusicInfo { mode }) =
    mode


toLevel : MusicInfo -> Int
toLevel (MusicInfo { level }) =
    level


toStringLevel : MusicInfo -> String
toStringLevel (MusicInfo { level }) =
    String.repeat level "◆" ++ String.repeat (8 - level) "◇"


toFullTime : MusicInfo -> Float
toFullTime (MusicInfo { fullTime }) =
    fullTime


toBpm : MusicInfo -> Int
toBpm (MusicInfo { bpm }) =
    bpm


toMaxCombo : MusicInfo -> Int
toMaxCombo (MusicInfo { maxCombo }) =
    maxCombo


toMaxScore : MusicInfo -> Int
toMaxScore (MusicInfo { maxScore }) =
    maxScore


toStringTime : Float -> String
toStringTime time =
    let
        sec =
            modBy 60 (Basics.round time)

        min =
            Basics.round time // 60
    in
    String.fromInt min ++ "分" ++ String.fromInt sec ++ "秒"
