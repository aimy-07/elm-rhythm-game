port module Page.Play.JudgeEffect exposing (JudgeEffect, keyDownEffectCmd, longEffectCmd, missEffectCmd, new)

import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.KeyStr as KeyStr exposing (KeyStr)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.LongNoteLine as LongNoteLine exposing (LongNoteLine)
import Page.Play.Note as Note exposing (Note)
import Page.Play.NotesPerLane as NotesPerLane exposing (NotesPerLane)


type alias JudgeEffect =
    { keyStr : String
    , judgeText : String
    , noteType : String
    }


new : KeyStr -> JudgeKind -> Bool -> JudgeEffect
new keyStr judgeKind isLong =
    { keyStr = keyStr
    , judgeText = JudgeKind.toStringJudgeKind judgeKind
    , noteType =
        if isLong then
            "LONG"

        else
            "SINGLE"
    }


keyDownEffectCmd : JudgeKind -> NotesPerLane -> Cmd msg
keyDownEffectCmd judgeKind notesPerLane =
    let
        isLong =
            NotesPerLane.maybeHeadNote notesPerLane
                |> Maybe.map (\note -> Note.isLong note)
                |> Maybe.withDefault False
    in
    Cmd.batch
        [ new (NotesPerLane.toKeyStr notesPerLane) judgeKind isLong
            |> playJudgeEffectAnim
        , new (NotesPerLane.toKeyStr notesPerLane) judgeKind isLong
            |> playJudgeEffectTextAnim
        ]


missEffectCmd : CurrentMusicTime -> NotesPerLane -> Cmd msg
missEffectCmd currentMusicTime notesPerLane =
    let
        isOverMiss =
            NotesPerLane.isOverMiss currentMusicTime notesPerLane
    in
    if isOverMiss then
        new (NotesPerLane.toKeyStr notesPerLane) JudgeKind.miss False
            |> playJudgeEffectTextAnim

    else
        Cmd.none


longEffectCmd : NotesPerLane -> Cmd msg
longEffectCmd notesPerLane =
    NotesPerLane.toMaybeLongNoteLine notesPerLane
        |> Maybe.map
            (\longNoteLine ->
                let
                    timeCounter =
                        LongNoteLine.toTimeCounter longNoteLine
                in
                if Basics.modBy LongNoteLine.longCountDuration timeCounter == 0 && timeCounter >= 0 then
                    new (NotesPerLane.toKeyStr notesPerLane) JudgeKind.invalid True
                        |> playJudgeEffectAnim

                else
                    Cmd.none
            )
        |> Maybe.withDefault Cmd.none


port playJudgeEffectAnim : JudgeEffect -> Cmd msg


port playJudgeEffectTextAnim : JudgeEffect -> Cmd msg
