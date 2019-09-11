module Page.Play.Score exposing
    ( Score
    , add
    , addLong
    , calcLongScore
    , init
    , unwrap
    )

import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.LongNoteLine as LongNoteLine exposing (LongNoteLine)
import Page.Play.NotesPerLane as NotesPerLane exposing (NotesPerLane)


type Score
    = Score Int


init : Score
init =
    Score 0


unwrap : Score -> Int
unwrap (Score score) =
    score


add : JudgeKind -> Score -> Score
add judgeKind (Score score) =
    if JudgeKind.isPerfect judgeKind then
        Score (score + perfectScore)

    else if JudgeKind.isGreat judgeKind then
        Score (score + greatScore)

    else if JudgeKind.isGood judgeKind then
        Score (score + goodScore)

    else
        Score score


addLong : Score -> Int -> Score
addLong (Score score) addingScore =
    Score (score + addingScore)


calcLongScore : NotesPerLane -> Int
calcLongScore notesPerLane =
    NotesPerLane.toMaybeLongNoteLine notesPerLane
        |> Maybe.map
            (\longNoteLine ->
                let
                    timeCounter =
                        LongNoteLine.toTimeCounter longNoteLine
                in
                if Basics.modBy 200 timeCounter == 0 && timeCounter >= 0 then
                    longScore

                else
                    0
            )
        |> Maybe.withDefault 0



-- CONST


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
