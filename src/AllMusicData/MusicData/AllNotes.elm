module AllMusicData.MusicData.AllNotes exposing
    ( AllNotes
    , computeMaxCombo
    , computeMaxScore
    , empty
    , headNote
    , headNotes
    , isEmpty
    , new
    , removeDisabledNotes
    , toList
    , update
    , updateKeyDown
    , updateKeyUp
    )

import Constants exposing (perfectScore)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge exposing (Judge(..))
import Page.Play.Key as Key exposing (Key)
import Page.Play.Lane exposing (Lane)
import Page.Play.Note as Note exposing (Note)


type AllNotes
    = AllNotes
        { laneS : List Note
        , laneD : List Note
        , laneF : List Note
        , laneJ : List Note
        , laneK : List Note
        , laneL : List Note
        }


type alias CsvData =
    List (List (Maybe Float))


new :
    { bpm : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    }
    -> CsvData
    -> AllNotes
new musicData csvData =
    AllNotes
        { laneS = createNotesPerLane musicData Key.S csvData
        , laneD = createNotesPerLane musicData Key.D csvData
        , laneF = createNotesPerLane musicData Key.F csvData
        , laneJ = createNotesPerLane musicData Key.J csvData
        , laneK = createNotesPerLane musicData Key.K csvData
        , laneL = createNotesPerLane musicData Key.L csvData
        }


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


empty : AllNotes
empty =
    AllNotes
        { laneS = []
        , laneD = []
        , laneF = []
        , laneJ = []
        , laneK = []
        , laneL = []
        }


isEmpty : AllNotes -> Bool
isEmpty allNotes =
    allNotes == empty


toList : AllNotes -> List Note
toList (AllNotes allNotes) =
    [ allNotes.laneS
    , allNotes.laneD
    , allNotes.laneF
    , allNotes.laneJ
    , allNotes.laneK
    , allNotes.laneL
    ]
        |> List.concat
        |> List.sortBy Note.toJustTime


toNotesPerLane : Key -> AllNotes -> List Note
toNotesPerLane key (AllNotes allNotes) =
    case key of
        Key.S ->
            allNotes.laneS

        Key.D ->
            allNotes.laneD

        Key.F ->
            allNotes.laneF

        Key.J ->
            allNotes.laneJ

        Key.K ->
            allNotes.laneK

        Key.L ->
            allNotes.laneL


updateNotesPerLane : Key -> (List Note -> List Note) -> AllNotes -> AllNotes
updateNotesPerLane key updateNotes (AllNotes allNotes) =
    case key of
        Key.S ->
            AllNotes { allNotes | laneS = updateNotes allNotes.laneS }

        Key.D ->
            AllNotes { allNotes | laneD = updateNotes allNotes.laneD }

        Key.F ->
            AllNotes { allNotes | laneF = updateNotes allNotes.laneF }

        Key.J ->
            AllNotes { allNotes | laneJ = updateNotes allNotes.laneJ }

        Key.K ->
            AllNotes { allNotes | laneK = updateNotes allNotes.laneK }

        Key.L ->
            AllNotes { allNotes | laneL = updateNotes allNotes.laneL }


updateAllNotes : (List Note -> List Note) -> AllNotes -> AllNotes
updateAllNotes updateNotes (AllNotes allNotes) =
    AllNotes
        { allNotes
            | laneS = updateNotes allNotes.laneS
            , laneD = updateNotes allNotes.laneD
            , laneF = updateNotes allNotes.laneF
            , laneJ = updateNotes allNotes.laneJ
            , laneK = updateNotes allNotes.laneK
            , laneL = updateNotes allNotes.laneL
        }


{-| 指定レーンにおいて、先頭にあるノーツを取得する
-}
headNote : Key -> AllNotes -> Maybe Note
headNote key allNotes =
    allNotes
        |> toNotesPerLane key
        |> List.head


{-| 全レーンにおいて、各レーンの先頭にあるノーツを取得する
-}
headNotes : AllNotes -> List Note
headNotes (AllNotes allNotes) =
    [ List.head allNotes.laneS
    , List.head allNotes.laneD
    , List.head allNotes.laneF
    , List.head allNotes.laneJ
    , List.head allNotes.laneK
    , List.head allNotes.laneL
    ]
        |> List.filterMap identity


{-| 毎フレームUpdateごとに、ノーツの状態を更新する
-}
update : CurrentMusicTime -> List Lane -> AllNotes -> AllNotes
update currentMusicTime lanes allNotes =
    allNotes
        |> updateAllNotes
            (\notesPerLane ->
                case notesPerLane of
                    head :: tails ->
                        Note.update currentMusicTime lanes head :: tails

                    [] ->
                        notesPerLane
            )


{-| KeyDown時に、判定結果に応じてノーツの状態を更新する
-}
updateKeyDown : Key -> Judge -> AllNotes -> AllNotes
updateKeyDown key judge allNotes =
    allNotes
        |> updateNotesPerLane key
            (\notesPerLane ->
                case notesPerLane of
                    head :: tails ->
                        Note.updateKeyDown judge head :: tails

                    [] ->
                        notesPerLane
            )


{-| ロングノーツの終端が判定バーに達する前にKeyUpした時、Lost判定にする
-}
updateKeyUp : Key -> AllNotes -> AllNotes
updateKeyUp key allNotes =
    allNotes
        |> updateNotesPerLane key
            (\notesPerLane ->
                case notesPerLane of
                    head :: tails ->
                        Note.updateKeyUp head :: tails

                    [] ->
                        notesPerLane
            )


{-| Disabledになったノーツを削除する
-}
removeDisabledNotes : AllNotes -> AllNotes
removeDisabledNotes allNotes =
    allNotes
        |> updateAllNotes
            (\notesPerLane ->
                case notesPerLane of
                    head :: tails ->
                        if Note.isDisabled head then
                            tails

                        else
                            head :: tails

                    [] ->
                        notesPerLane
            )


{-| 総スコアの計算
-}
computeMaxScore : AllNotes -> Int
computeMaxScore (AllNotes allNotes) =
    let
        computeMaxScorePerLane notesPerLine =
            notesPerLine
                |> List.map (\note -> 1 + List.length (Note.toLongSubNotes note))
                |> List.sum
                |> (*) perfectScore
    in
    [ computeMaxScorePerLane allNotes.laneS
    , computeMaxScorePerLane allNotes.laneD
    , computeMaxScorePerLane allNotes.laneF
    , computeMaxScorePerLane allNotes.laneJ
    , computeMaxScorePerLane allNotes.laneK
    , computeMaxScorePerLane allNotes.laneL
    ]
        |> List.sum


computeMaxCombo : AllNotes -> Int
computeMaxCombo (AllNotes allNotes) =
    let
        computeMaxComboPerLane notesPerLine =
            notesPerLine
                |> List.map (\note -> 1 + List.length (Note.toLongSubNotes note))
                |> List.sum
    in
    [ computeMaxComboPerLane allNotes.laneS
    , computeMaxComboPerLane allNotes.laneD
    , computeMaxComboPerLane allNotes.laneF
    , computeMaxComboPerLane allNotes.laneJ
    , computeMaxComboPerLane allNotes.laneK
    , computeMaxComboPerLane allNotes.laneL
    ]
        |> List.sum
