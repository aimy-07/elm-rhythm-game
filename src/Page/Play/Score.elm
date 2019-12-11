module Page.Play.Score exposing
    ( Score
    , init
    , unwrap
    , update
    , updateKeyDown
    )

import Constants exposing (goodScore, greatScore, longScore, perfectScore)
import Page.Play.Judge exposing (Judge(..))


type Score
    = Score Int


init : Score
init =
    Score 0


unwrap : Score -> Int
unwrap (Score score) =
    score


update : Int -> Score -> Score
update judgedLongNoteCount (Score score) =
    Score (score + judgedLongNoteCount * longScore)


updateKeyDown : Judge -> Score -> Score
updateKeyDown judge (Score score) =
    case judge of
        Perfect ->
            Score (score + perfectScore)

        Great ->
            Score (score + greatScore)

        Good ->
            Score (score + goodScore)

        Miss ->
            Score score

        Invalid ->
            Score score
