module Page.Play.Score exposing (Score, add, addLong, init, longScore, toString)

import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)


type Score
    = Score Int


init : Score
init =
    Score 0


toString : Score -> String
toString (Score score) =
    String.fromInt score


add : JudgeKind -> Score -> Score
add judgeKind (Score score) =
    if JudgeKind.isPerfect judgeKind then
        Score (score + perfectScore)

    else if JudgeKind.isGreat judgeKind then
        Score (score + greatScore)

    else if JudgeKind.isGood judgeKind then
        Score (score + goodScore)

    else
        Score score


addLong : Score -> Int -> Score
addLong (Score score) addingScore =
    Score (score + addingScore)



-- CONST


perfectScore : Int
perfectScore =
    2000


greatScore : Int
greatScore =
    1500


goodScore : Int
goodScore =
    1000


longScore : Int
longScore =
    100
