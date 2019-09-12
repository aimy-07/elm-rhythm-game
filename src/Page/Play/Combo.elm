module Page.Play.Combo exposing
    ( Combo
    , addLong
    , calcLongCombo
    , init
    , isZero
    , unwrap
    , update
    )

import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.LongNoteLine as LongNoteLine exposing (LongNoteLine)
import Page.Play.NotesPerLane as NotesPerLane exposing (NotesPerLane)


type Combo
    = Combo Int


init : Combo
init =
    Combo 0


unwrap : Combo -> Int
unwrap (Combo combo) =
    combo


update : JudgeKind -> Combo -> Combo
update judgeKind (Combo combo) =
    if JudgeKind.isPerfect judgeKind || JudgeKind.isGreat judgeKind || JudgeKind.isGood judgeKind then
        Combo (combo + 1)

    else if judgeKind == JudgeKind.miss then
        Combo 0

    else
        Combo combo


addLong : Combo -> Int -> Combo
addLong (Combo combo) addingCombo =
    Combo (combo + addingCombo)


calcLongCombo : NotesPerLane -> Int
calcLongCombo notesPerLane =
    NotesPerLane.toMaybeLongNoteLine notesPerLane
        |> Maybe.map
            (\longNoteLine ->
                let
                    timeCounter =
                        LongNoteLine.toTimeCounter longNoteLine
                in
                if Basics.modBy LongNoteLine.longCountDuration timeCounter == 0 && timeCounter >= 0 then
                    1

                else
                    0
            )
        |> Maybe.withDefault 0


isZero : Combo -> Bool
isZero (Combo combo) =
    combo == 0
