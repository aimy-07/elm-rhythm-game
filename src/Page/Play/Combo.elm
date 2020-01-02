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


update : Bool -> Int -> Combo -> Combo
update hasDisabledNotes judgedLongNoteCount (Combo combo resultCombo) =
    let
        -- Disabledノーツがあったら先にCombo = 0を実行する
        nextCombo =
            if hasDisabledNotes then
                1 * judgedLongNoteCount

            else
                combo + judgedLongNoteCount
    in
    Combo nextCombo (updateResultCombo nextCombo resultCombo)


updateKeyDown : Judge -> Combo -> Combo
updateKeyDown judge (Combo combo resultCombo) =
    let
        -- Disabledノーツがあったら先にCombo = 0を実行する
        nextCombo =
            case judge of
                Perfect ->
                    combo + 1

                Great ->
                    combo + 1

                Good ->
                    combo + 1

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
