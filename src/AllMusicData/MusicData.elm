module AllMusicData.MusicData exposing
    ( MusicData
    , MusicDataCsvDto
    , MusicDataJsonDto
    , empty
    , newFromJson
    , toStringTime
    , updateFromCsv
    )

import AllMusicData.MusicData.CsvFileName as CsvFileName exposing (CsvFileName)
import AllMusicData.MusicData.Level exposing (Level)
import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)
import Constants exposing (allKeyList)
import Page.Play.Note as Note exposing (Note)


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
    , allNotes : List Note
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
        { normal : Int
        , hard : Int
        , master : Int
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
                Mode.Normal ->
                    jsonDto.level.normal

                Mode.Hard ->
                    jsonDto.level.hard

                Mode.Master ->
                    jsonDto.level.master
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
    , allNotes = []
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
                    csvData
                        |> List.map
                            (createNotesFromCsvRow
                                { beatsCountPerMeasure = Basics.toFloat musicData.beatsCountPerMeasure
                                , timePerBeat = 60 * 1000 / Basics.toFloat musicData.bpm
                                , offset = musicData.offset
                                }
                            )
                        |> List.concat

                maxCombo =
                    Note.computeMaxCombo allNotes

                maxScore =
                    Note.computeMaxScore allNotes
            in
            Just { musicData | allNotes = allNotes, maxScore = maxScore, maxCombo = maxCombo }

        Nothing ->
            maybeMusicData


createNotesFromCsvRow :
    { beatsCountPerMeasure : Float
    , timePerBeat : Float
    , offset : Float
    }
    -> List (Maybe Float)
    -> List Note
createNotesFromCsvRow { beatsCountPerMeasure, timePerBeat, offset } csvRow =
    let
        maybeMeasure =
            csvRow
                |> List.head
                |> Maybe.withDefault Nothing

        maybeBeat =
            csvRow
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault Nothing

        notesData =
            csvRow
                |> List.drop 2
    in
    let
        maybeJustTime =
            case ( maybeMeasure, maybeBeat ) of
                ( Just measure, Just beat ) ->
                    Just <| ((measure * beatsCountPerMeasure + beat) * timePerBeat) + offset * 1000

                _ ->
                    Nothing
    in
    notesData
        |> List.indexedMap
            (\index maybeValue ->
                let
                    maybeKey =
                        allKeyList
                            |> List.drop index
                            |> List.head

                    maybeLongTime =
                        case maybeValue of
                            Just value ->
                                if value == Basics.toFloat 0 then
                                    maybeValue

                                else
                                    Just (value * timePerBeat)

                            Nothing ->
                                maybeValue
                in
                case ( maybeKey, maybeJustTime, maybeLongTime ) of
                    ( Just key, Just justTime, Just longTime ) ->
                        Just <|
                            Note.new
                                { key = key
                                , justTime = justTime
                                , longTime = longTime
                                }

                    _ ->
                        Nothing
            )
        |> List.filterMap identity


empty : MusicData
empty =
    { csvFileName = ""
    , musicId = ""
    , musicName = ""
    , composer = ""
    , mode = Mode.Normal
    , level = 0
    , fullTime = 0
    , bpm = 0
    , beatsCountPerMeasure = 0
    , offset = 0
    , allNotes = []
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
