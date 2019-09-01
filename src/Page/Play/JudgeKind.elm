module Page.Play.JudgeKind exposing (JudgeKind, invalid, isGood, isGreat, isInvalid, isOverMiss, isPerfect, judgeKeyDown, miss, toStringJudgeKind)

import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Note as Note exposing (JustTime, Note)


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


judgeKeyDown : CurrentMusicTime -> Maybe Note -> JudgeKind
judgeKeyDown currentMusicTime maybeNote =
    maybeNote
        |> Maybe.map
            (\note ->
                let
                    justTime =
                        Note.toJustTime note
                in
                if Basics.abs (justTime - currentMusicTime) < perfectRange then
                    Perfect

                else if Basics.abs (justTime - currentMusicTime) < greatRange then
                    Great

                else if Basics.abs (justTime - currentMusicTime) < goodRange then
                    Good

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


isOverMiss : CurrentMusicTime -> Maybe Note -> Bool
isOverMiss currentMusicTime maybeNote =
    maybeNote
        |> Maybe.map
            (\note ->
                let
                    justTime =
                        Note.toJustTime note
                in
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
