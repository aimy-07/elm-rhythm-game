module Page.Play.Combo exposing
    ( Combo
    , comboEffectCmd
    , init
    , toMaxCombo
    , unwrap
    , update
    , updateKeyDown
    )

import AnimationManager
import Page.Play.Judge exposing (Judge(..))
import Utils exposing (cmdIf)


type Combo
    = Combo Int MaxCombo



-- TODO: 名前が紛らわしいので変える
-- 意味的には、連続した最大コンボ


type alias MaxCombo =
    Int


init : Combo
init =
    Combo 0 0


unwrap : Combo -> Int
unwrap (Combo combo _) =
    combo


toMaxCombo : Combo -> MaxCombo
toMaxCombo (Combo _ maxCombo) =
    maxCombo


update : Bool -> Int -> Combo -> Combo
update hasDisabledNotes judgedLongNoteCount (Combo combo maxCombo) =
    let
        -- Disabledノーツがあったら先にCombo = 0を実行する
        nextCombo =
            if hasDisabledNotes then
                1 * judgedLongNoteCount

            else
                combo + judgedLongNoteCount
    in
    Combo nextCombo (updateMaxCombo nextCombo maxCombo)


updateKeyDown : Judge -> Combo -> Combo
updateKeyDown judge (Combo combo maxCombo) =
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
    Combo nextCombo (updateMaxCombo nextCombo maxCombo)


updateMaxCombo : Int -> MaxCombo -> MaxCombo
updateMaxCombo combo maxCombo =
    if combo > maxCombo then
        combo

    else
        maxCombo


comboEffectCmd : Combo -> Combo -> Cmd msg
comboEffectCmd prevCombo nextCombo =
    AnimationManager.playComboEffectAnim ()
        |> cmdIf (unwrap nextCombo > unwrap prevCombo)
