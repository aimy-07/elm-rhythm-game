module Page.Play.Score exposing
    ( Score
    , init
    , unwrap
    , update
    , updateKeyDown
    )

import Constants exposing (goodScore, niceScore, perfectScore)
import Page.Play.Judge exposing (Judge(..))
import Page.Play.Note as Note exposing (Note)


type Score
    = Score Int


init : Score
init =
    Score 0


unwrap : Score -> Int
unwrap (Score score) =
    score


update : List Note -> Score -> Score
update headNotes (Score score) =
    let
        headNoteJudges =
            List.map Note.headNoteJudge headNotes

        increment =
            headNoteJudges
                |> List.map
                    (\judge ->
                        case judge of
                            Perfect ->
                                perfectScore

                            Nice ->
                                niceScore

                            Good ->
                                goodScore

                            Lost ->
                                0

                            Miss ->
                                0

                            Invalid ->
                                0
                    )
                |> List.sum
    in
    Score (score + increment)


updateKeyDown : Judge -> Score -> Score
updateKeyDown judge (Score score) =
    case judge of
        Perfect ->
            Score (score + perfectScore)

        Nice ->
            Score (score + niceScore)

        Good ->
            Score (score + goodScore)

        Lost ->
            -- KeyDownでLost判定が出ることはない
            Score score

        Miss ->
            Score score

        Invalid ->
            Score score
