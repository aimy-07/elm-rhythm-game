module Page.Play.Combo exposing
    ( Combo
    , comboEffectCmd
    , init
    , toResultCombo
    , unwrap
    , update
    , updateKeyDown
    )

import AnimationManager
import Page.Play.Judge exposing (Judge(..))
import Page.Play.Note as Note exposing (Note)
import Utils exposing (cmdIf)


type Combo
    = Combo
        { combo : Int
        , resultCombo : Int
        }


init : Combo
init =
    Combo
        { combo = 0
        , resultCombo = 0
        }


unwrap : Combo -> Int
unwrap (Combo { combo }) =
    combo


toResultCombo : Combo -> Int
toResultCombo (Combo { resultCombo }) =
    resultCombo


update : List Note -> Combo -> Combo
update headNotes (Combo { combo, resultCombo }) =
    let
        headNoteJudges =
            List.map Note.headNoteJudge headNotes

        comboIncrement =
            headNoteJudges
                |> List.map computeComboIncrement
                |> List.sum

        nextCombo =
            if List.member Miss headNoteJudges then
                -- Missがあったらそれまでのcomboは無効にする
                comboIncrement

            else
                combo + comboIncrement
    in
    Combo
        { combo = nextCombo
        , resultCombo = updateResultCombo nextCombo resultCombo
        }


updateKeyDown : Judge -> Combo -> Combo
updateKeyDown judge (Combo { combo, resultCombo }) =
    let
        comboIncrement =
            computeComboIncrement judge

        nextCombo =
            if judge == Miss then
                0

            else
                combo + comboIncrement
    in
    Combo
        { combo = nextCombo
        , resultCombo = updateResultCombo nextCombo resultCombo
        }


computeComboIncrement : Judge -> Int
computeComboIncrement judge =
    case judge of
        Perfect ->
            1

        Nice ->
            1

        Good ->
            1

        Lost ->
            -- Lost：Comboが0にはならないが、Combo数は増えない
            0

        Miss ->
            0

        Invalid ->
            0


updateResultCombo : Int -> Int -> Int
updateResultCombo combo resultCombo =
    if combo > resultCombo then
        combo

    else
        resultCombo


comboEffectCmd : Combo -> Combo -> Cmd msg
comboEffectCmd prevCombo nextCombo =
    AnimationManager.playComboEffectAnim ()
        |> cmdIf (unwrap nextCombo > unwrap prevCombo)
