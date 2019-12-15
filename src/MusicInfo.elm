module MusicInfo exposing
    ( MusicInfo
    , MusicInfoDto
    , new
    , toStringTime
    )

import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Level exposing (Level)
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)


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

    -- , publicRecord : PublicRecord
    }



-- type alias PublicRecord =
--     { first : { uid : Uid, score : Int }
--     , second : { uid : Uid, score : Int }
--     , third : { uid : Uid, score : Int }
--     }


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

    -- , publicRecord
    --     { first : {uid: Uid, score : Int}
    --     , second : {uid: Uid, score : Int}
    --     , third : {uid: Uid, score : Int}
    --     }
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

    -- , publicRecord = musicInfoDto.publicRecord
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
