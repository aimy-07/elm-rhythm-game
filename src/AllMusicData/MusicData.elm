module AllMusicData.MusicData exposing
    ( MusicData
    , MusicDataCsvDto
    , MusicDataJsonDto
    , empty
    , newFromJson
    , toStringTime
    , updateFromCsv
    )

import AllMusicData.MusicData.AllNotes as AllNotes exposing (AllNotes)
import AllMusicData.MusicData.CsvFileName as CsvFileName exposing (CsvFileName)
import AllMusicData.MusicData.Level exposing (Level)
import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)


type alias MusicData =
    { csvFileName : CsvFileName
    , musicId : MusicId
    , musicName : String
    , composer : String
    , mode : Mode
    , level : Level
    , fullTime : Float
    , bpm : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    , allNotes : AllNotes
    , maxCombo : Int
    , maxScore : Int
    , order : Int
    }


type alias MusicDataJsonDto =
    { musicId : String
    , musicName : String
    , composer : String
    , fullTime : Float
    , bpm : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    , level :
        { easy : Int
        , normal : Int
        , hard : Int
        }
    , order : Int
    }


type alias MusicDataCsvDto =
    { csvFileName : String
    , csvData : CsvData
    }


type alias CsvData =
    List (List (Maybe Float))


newFromJson : MusicDataJsonDto -> Mode -> MusicData
newFromJson jsonDto mode =
    let
        level =
            case mode of
                Mode.Easy ->
                    jsonDto.level.easy

                Mode.Normal ->
                    jsonDto.level.normal

                Mode.Hard ->
                    jsonDto.level.hard
    in
    { csvFileName = CsvFileName.create jsonDto.musicId mode
    , musicId = jsonDto.musicId
    , musicName = jsonDto.musicName
    , composer = jsonDto.composer
    , mode = mode
    , level = level
    , fullTime = jsonDto.fullTime
    , bpm = jsonDto.bpm
    , beatsCountPerMeasure = jsonDto.beatsCountPerMeasure
    , offset = jsonDto.offset
    , allNotes = AllNotes.empty
    , maxCombo = 0
    , maxScore = 0
    , order = jsonDto.order
    }


updateFromCsv : CsvData -> Maybe MusicData -> Maybe MusicData
updateFromCsv csvData maybeMusicData =
    case maybeMusicData of
        Just musicData ->
            let
                allNotes =
                    AllNotes.new
                        { bpm = musicData.bpm
                        , beatsCountPerMeasure = musicData.beatsCountPerMeasure
                        , offset = musicData.offset
                        }
                        csvData

                maxCombo =
                    AllNotes.computeMaxCombo allNotes

                maxScore =
                    AllNotes.computeMaxScore allNotes
            in
            Just <|
                { musicData
                    | allNotes = allNotes
                    , maxCombo = maxCombo
                    , maxScore = maxScore
                }

        Nothing ->
            maybeMusicData


empty : MusicData
empty =
    { csvFileName = ""
    , musicId = ""
    , musicName = ""
    , composer = ""
    , mode = Mode.Easy
    , level = 0
    , fullTime = 0
    , bpm = 0
    , beatsCountPerMeasure = 0
    , offset = 0
    , allNotes = AllNotes.empty
    , maxCombo = 0
    , maxScore = 0
    , order = -1
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
