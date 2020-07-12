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
    = Combo Int ResultCombo


type alias ResultCombo =
    Int


init : Combo
init =
    Combo 0 0


unwrap : Combo -> Int
unwrap (Combo combo _) =
    combo


toResultCombo : Combo -> ResultCombo
toResultCombo (Combo _ resultCombo) =
    resultCombo


update : List Note -> Combo -> Combo
update headNotes (Combo combo resultCombo) =
    let
        headNoteJudges =
            List.map Note.headNoteJudge headNotes

        increment =
            headNoteJudges
                |> List.map
                    (\judge ->
                        case judge of
                            Perfect ->
                                1

                            Nice ->
                                1

                            Good ->
                                1

                            Lost ->
                                0

                            Miss ->
                                0

                            Invalid ->
                                0
                    )
                |> List.sum

        nextCombo =
            if List.member Miss headNoteJudges then
                -- Missがあったらそれまでのcomboは無効にする
                increment

            else
                combo + increment
    in
    Combo nextCombo (updateResultCombo nextCombo resultCombo)


updateKeyDown : Judge -> Combo -> Combo
updateKeyDown judge (Combo combo resultCombo) =
    let
        nextCombo =
            case judge of
                Perfect ->
                    combo + 1

                Nice ->
                    combo + 1

                Good ->
                    combo + 1

                Lost ->
                    -- KeyDownでLost判定が出ることはない
                    combo

                Miss ->
                    0

                Invalid ->
                    combo
    in
    Combo nextCombo (updateResultCombo nextCombo resultCombo)


updateResultCombo : Int -> ResultCombo -> ResultCombo
updateResultCombo combo resultCombo =
    if combo > resultCombo then
        combo

    else
        resultCombo


comboEffectCmd : Combo -> Combo -> Cmd msg
comboEffectCmd prevCombo nextCombo =
    AnimationManager.playComboEffectAnim ()
        |> cmdIf (unwrap nextCombo > unwrap prevCombo)
