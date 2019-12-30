port module MusicInfo exposing
    ( MusicInfo
    , MusicInfoDto
    , empty
    , getAllNotes
    , gotAllNotes
    , new
    , toStringTime
    )

import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Level exposing (Level)
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import Page.Play.Note exposing (NoteDto)


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
    }


empty : MusicInfo
empty =
    { musicId = ""
    , csvFileName = ""
    , musicName = ""
    , composer = ""
    , mode = Mode.invalid
    , level = 0
    , fullTime = 0
    , bpm = 0
    , maxCombo = 0
    , maxScore = 0
    , beatsCountPerMeasure = 0
    , offset = 0
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



-- TODO: Csvのモジュールを作ったらそこにおく


port getAllNotes : { csvFileName : String, bpm : Int, beatsCountPerMeasure : Int, offset : Float } -> Cmd msg


port gotAllNotes : (List NoteDto -> msg) -> Sub msg
