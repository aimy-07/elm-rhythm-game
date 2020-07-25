module AllMusicData.MusicData.Csv exposing
    ( CsvData
    , CsvDto
    , computeMaxBasicScore
    , computeMaxCombo
    , computeMaxComboBonus
    , createAllNotes
    , createNotesPerLane
    )

import Constants exposing (comboBonusRate, perfectScore)
import Page.Play.Judge exposing (Judge(..))
import Page.Play.Key as Key exposing (Key)
import Page.Play.Note as Note exposing (Note)


type alias CsvDto =
    { csvFileName : String
    , csvData : CsvData
    }


type alias CsvData =
    List (List (Maybe Float))


createNotesPerLane :
    { bpm : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    }
    -> Key
    -> CsvData
    -> List Note
createNotesPerLane { bpm, beatsCountPerMeasure, offset } key csvData =
    let
        timePerBeat =
            60 * 1000 / Basics.toFloat bpm
    in
    csvData
        |> List.filterMap
            (\csvRow ->
                let
                    csvCellData index =
                        (List.drop index >> List.head >> Maybe.withDefault Nothing) csvRow

                    maybeMeasure =
                        csvCellData 0

                    maybeBeat =
                        csvCellData 1

                    maybeNoteData =
                        case key of
                            Key.S ->
                                csvCellData 2

                            Key.D ->
                                csvCellData 3

                            Key.F ->
                                csvCellData 4

                            Key.J ->
                                csvCellData 5

                            Key.K ->
                                csvCellData 6

                            Key.L ->
                                csvCellData 7
                in
                case ( maybeMeasure, maybeBeat, maybeNoteData ) of
                    ( Just measure, Just beat, Just noteData ) ->
                        let
                            justTime =
                                ((measure * Basics.toFloat beatsCountPerMeasure + beat) * timePerBeat) + offset * 1000

                            longTime =
                                noteData * timePerBeat
                        in
                        Just <|
                            Note.new
                                { key = key
                                , justTime = justTime
                                , longTime = longTime
                                }

                    _ ->
                        Nothing
            )
        |> List.sortBy Note.toJustTime


createAllNotes :
    { bpm : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    }
    -> CsvData
    -> List Note
createAllNotes musicData csvData =
    [ createNotesPerLane musicData Key.S csvData
    , createNotesPerLane musicData Key.D csvData
    , createNotesPerLane musicData Key.F csvData
    , createNotesPerLane musicData Key.J csvData
    , createNotesPerLane musicData Key.K csvData
    , createNotesPerLane musicData Key.L csvData
    ]
        |> List.concat


{-| 総基本スコアの計算
-}
computeMaxBasicScore : List Note -> Int
computeMaxBasicScore notes =
    notes
        |> List.map (\note -> 1 + List.length (Note.toLongSubNotes note))
        |> List.sum
        |> (*) perfectScore


{-| 総基本コンボボーナススコアの計算
-}
computeMaxComboBonus : List Note -> Int
computeMaxComboBonus notes =
    notes
        |> computeMaxBasicScore
        |> Basics.toFloat
        |> (*) comboBonusRate
        |> Basics.floor


{-| 総コンボの計算
-}
computeMaxCombo : List Note -> Int
computeMaxCombo notes =
    notes
        |> List.map (\note -> 1 + List.length (Note.toLongSubNotes note))
        |> List.sum
