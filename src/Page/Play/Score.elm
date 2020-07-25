module Page.Play.Score exposing
    ( Score
    , init
    , unwrap
    , update
    , updateKeyDown
    )

import Constants exposing (comboBonusActivateCombo, goodScore, niceScore, perfectScore)
import Page.Play.Judge exposing (Judge(..))
import Page.Play.Note as Note exposing (Note)


type Score
    = Score
        { basicScore : Int
        , comboBonus : Float
        , comboBonusPerNote : Float
        }


init : Int -> Int -> Score
init maxCombo maxComboBonus =
    Score
        { basicScore = 0
        , comboBonus = 0
        , comboBonusPerNote = Basics.toFloat maxComboBonus / Basics.toFloat (maxCombo - comboBonusActivateCombo + 1)
        }


unwrap : Score -> Int
unwrap (Score { basicScore, comboBonus }) =
    basicScore + Basics.floor comboBonus


update : List Note -> Int -> Score -> Score
update headNotes combo (Score score) =
    let
        headNoteJudges =
            List.map Note.headNoteJudge headNotes

        scoreIncrement =
            headNoteJudges
                |> List.map computeBasicScoreIncrement
                |> List.sum

        comboBonusIncrement =
            headNoteJudges
                |> List.map (computeComboBonusIncrement score.comboBonusPerNote combo)
                |> List.sum
    in
    Score
        { score
            | basicScore = score.basicScore + scoreIncrement
            , comboBonus = score.comboBonus + comboBonusIncrement
        }


updateKeyDown : Judge -> Int -> Score -> Score
updateKeyDown judge combo (Score score) =
    let
        scoreIncrement =
            computeBasicScoreIncrement judge

        comboBonusIncrement =
            computeComboBonusIncrement score.comboBonusPerNote combo judge
    in
    Score
        { score
            | basicScore = score.basicScore + scoreIncrement
            , comboBonus = score.comboBonus + comboBonusIncrement
        }


computeBasicScoreIncrement : Judge -> Int
computeBasicScoreIncrement judge =
    case judge of
        Perfect ->
            perfectScore

        Nice ->
            niceScore

        Good ->
            goodScore

        Lost ->
            -- Lost：Comboが0にはならないが、Combo数は増えない
            0

        Miss ->
            0

        Invalid ->
            0


computeComboBonusIncrement : Float -> Int -> Judge -> Float
computeComboBonusIncrement comboBonusPerNote combo judge =
    if combo >= comboBonusActivateCombo then
        case judge of
            Perfect ->
                comboBonusPerNote

            Nice ->
                comboBonusPerNote

            Good ->
                comboBonusPerNote

            Lost ->
                -- Lost：Comboが0にはならないが、Combo数は増えない
                0

            Miss ->
                0

            Invalid ->
                0

    else
        0
