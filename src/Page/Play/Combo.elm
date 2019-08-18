module Page.Play.Combo exposing (Combo, init, toString, update)

import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)


type Combo
    = Combo Int


init : Combo
init =
    Combo 0


toString : Combo -> String
toString (Combo combo) =
    String.fromInt combo


update : JudgeKind -> Combo -> Combo
update judgeKind (Combo combo) =
    if JudgeKind.isPerfect judgeKind || JudgeKind.isGreat judgeKind || JudgeKind.isGood judgeKind then
        Combo (combo + 1)

    else if judgeKind == JudgeKind.miss then
        Combo 0

    else
        Combo combo
