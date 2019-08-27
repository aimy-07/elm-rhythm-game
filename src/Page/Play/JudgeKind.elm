module Page.Play.JudgeKind exposing (JudgeKind, invalid, isGood, isGreat, isInvalid, isOverMiss, isPerfect, judgeKeyDown, miss, toStringJudgeKind)

import Page.Play.ConcurrentNotes as ConcurrentNotes exposing (ConcurrentNotes)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JustTime exposing (JustTime)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.Note as Note exposing (Note)


type JudgeKind
    = Perfect
    | Great
    | Good
    | Miss
    | Invalid


toStringJudgeKind : JudgeKind -> String
toStringJudgeKind judgeKind =
    case judgeKind of
        Perfect ->
            "Perfect"

        Great ->
            "Great"

        Good ->
            "Good"

        Miss ->
            "Miss"

        Invalid ->
            ""


judgeKeyDown : CurrentMusicTime -> LinePosition -> Maybe ConcurrentNotes -> JudgeKind
judgeKeyDown currentMusicTime position maybeHead =
    maybeHead
        |> Maybe.map
            (\head ->
                let
                    justTime =
                        ConcurrentNotes.toJustTime head

                    notes =
                        ConcurrentNotes.toNotes head

                    isCorrectKey =
                        List.any (\note -> Note.toPosition note == position) notes
                in
                if isCorrectKey then
                    if Basics.abs (justTime - currentMusicTime) < perfectRange then
                        Perfect

                    else if Basics.abs (justTime - currentMusicTime) < greatRange then
                        Great

                    else if Basics.abs (justTime - currentMusicTime) < goodRange then
                        Good

                    else
                        Invalid

                else
                    Invalid
            )
        |> Maybe.withDefault Invalid


isInvalid : JudgeKind -> Bool
isInvalid judgeKind =
    judgeKind == Invalid


isPerfect : JudgeKind -> Bool
isPerfect judgeKind =
    judgeKind == Perfect


isGreat : JudgeKind -> Bool
isGreat judgeKind =
    judgeKind == Great


isGood : JudgeKind -> Bool
isGood judgeKind =
    judgeKind == Good


isOverMiss : CurrentMusicTime -> Maybe JustTime -> Bool
isOverMiss currentMusicTime maybeJustTime =
    maybeJustTime
        |> Maybe.map
            (\justTime ->
                (currentMusicTime - justTime) > goodRange
            )
        |> Maybe.withDefault False


miss : JudgeKind
miss =
    Miss


invalid : JudgeKind
invalid =
    Invalid



-- CONST


perfectRange : Float
perfectRange =
    40


greatRange : Float
greatRange =
    80


goodRange : Float
goodRange =
    120
