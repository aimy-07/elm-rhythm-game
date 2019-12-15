module MusicInfo exposing
    ( MusicInfo
    , MusicInfoDto
    , new
    , toFirstRecord
    , toSecondRecord
    , toStringTime
    , toThirdRecord
    )

import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Level exposing (Level)
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import PublicRecord exposing (PublicRecord, PublicRecordDto)


type alias MusicInfo =
    { musicId : MusicId
    , csvFileName : CsvFileName
    , musicName : String
    , composer : String
    , mode : Mode
    , level : Level
    , fullTime : Float
    , bpm : Int
    , maxCombo : Int
    , maxScore : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    , bestRecords : List PublicRecord
    }


type alias MusicInfoDto =
    { musicId : String
    , csvFileName : String
    , musicName : String
    , composer : String
    , mode : String
    , level : Int
    , fullTime : Float
    , bpm : Int
    , maxCombo : Int
    , maxScore : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    , bestRecords : List PublicRecordDto
    }


new : MusicInfoDto -> MusicInfo
new musicInfoDto =
    { musicId = musicInfoDto.musicId
    , csvFileName = musicInfoDto.csvFileName
    , musicName = musicInfoDto.musicName
    , composer = musicInfoDto.composer
    , mode = Mode.new musicInfoDto.mode
    , level = musicInfoDto.level
    , fullTime = musicInfoDto.fullTime
    , bpm = musicInfoDto.bpm
    , maxCombo = musicInfoDto.maxCombo
    , maxScore = musicInfoDto.maxScore
    , beatsCountPerMeasure = musicInfoDto.beatsCountPerMeasure
    , offset = musicInfoDto.offset
    , bestRecords = musicInfoDto.bestRecords
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


toFirstRecord : MusicInfo -> Maybe PublicRecord
toFirstRecord musicInfo =
    musicInfo.bestRecords
        |> List.sortBy .bestScore
        |> List.reverse
        |> List.head


toSecondRecord : MusicInfo -> Maybe PublicRecord
toSecondRecord musicInfo =
    musicInfo.bestRecords
        |> List.sortBy .bestScore
        |> List.reverse
        |> List.drop 1
        |> List.head


toThirdRecord : MusicInfo -> Maybe PublicRecord
toThirdRecord musicInfo =
    musicInfo.bestRecords
        |> List.sortBy .bestScore
        |> List.reverse
        |> List.drop 2
        |> List.head
