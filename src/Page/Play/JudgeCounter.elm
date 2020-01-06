module Page.Play.JudgeCounter exposing
    ( JudgeCounter
    , init
    , toGood
    , toGreat
    , toLost
    , toMiss
    , toPerfect
    , update
    , updateKeyDown
    )

import Page.Play.Judge exposing (Judge(..))
import Page.Play.Note as Note exposing (Note)


type JudgeCounter
    = JudgeCounter
        { perfect : Int
        , great : Int
        , good : Int
        , lost : Int
        , miss : Int
        }


init : JudgeCounter
init =
    JudgeCounter
        { perfect = 0
        , great = 0
        , good = 0
        , lost = 0
        , miss = 0
        }


update : List Note -> JudgeCounter -> JudgeCounter
update headNotes (JudgeCounter judgeCounter) =
    let
        headNoteJudges =
            List.map Note.headNoteJudge headNotes
    in
    JudgeCounter
        { judgeCounter
            | perfect = judgeCounter.perfect + computeIncrement Perfect headNoteJudges
            , great = judgeCounter.great + computeIncrement Great headNoteJudges
            , good = judgeCounter.good + computeIncrement Good headNoteJudges
            , lost = judgeCounter.lost + computeIncrement Lost headNoteJudges
            , miss = judgeCounter.miss + computeIncrement Miss headNoteJudges
        }


computeIncrement : Judge -> List Judge -> Int
computeIncrement targetJudge headNoteJudges =
    headNoteJudges
        |> List.map
            (\judge ->
                if judge == targetJudge then
                    1

                else
                    0
            )
        |> List.sum


updateKeyDown : Judge -> JudgeCounter -> JudgeCounter
updateKeyDown judge (JudgeCounter judgeCounter) =
    case judge of
        Perfect ->
            JudgeCounter { judgeCounter | perfect = judgeCounter.perfect + 1 }

        Great ->
            JudgeCounter { judgeCounter | great = judgeCounter.great + 1 }

        Good ->
            JudgeCounter { judgeCounter | good = judgeCounter.good + 1 }

        Lost ->
            JudgeCounter { judgeCounter | lost = judgeCounter.lost + 1 }

        Miss ->
            JudgeCounter { judgeCounter | miss = judgeCounter.miss + 1 }

        Invalid ->
            JudgeCounter judgeCounter


toPerfect : JudgeCounter -> Int
toPerfect (JudgeCounter { perfect }) =
    perfect


toGreat : JudgeCounter -> Int
toGreat (JudgeCounter { great }) =
    great


toGood : JudgeCounter -> Int
toGood (JudgeCounter { good }) =
    good


toLost : JudgeCounter -> Int
toLost (JudgeCounter { lost }) =
    lost


toMiss : JudgeCounter -> Int
toMiss (JudgeCounter { miss }) =
    miss
