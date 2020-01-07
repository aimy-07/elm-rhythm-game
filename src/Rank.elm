module Rank exposing
    ( Rank
    , allRankList
    , comboBorder
    , invalid
    , newComboRank
    , newScoreRank
    , scoreBorder
    , toString
    )


type Rank
    = SSS
    | SS
    | S
    | A
    | B
    | C
    | D
    | Invalid


newComboRank : Int -> Int -> Rank
newComboRank num max =
    if num >= comboBorder max SSS then
        SSS

    else if num >= comboBorder max SS then
        SS

    else if num >= comboBorder max S then
        S

    else if num >= comboBorder max A then
        A

    else if num >= comboBorder max B then
        B

    else if num >= comboBorder max C then
        C

    else if num >= comboBorder max D then
        D

    else
        Invalid


comboBorder : Int -> Rank -> Int
comboBorder max rank =
    case rank of
        SSS ->
            max

        SS ->
            Basics.round (Basics.toFloat max * 0.95)

        S ->
            Basics.round (Basics.toFloat max * 0.9)

        A ->
            Basics.round (Basics.toFloat max * 0.8)

        B ->
            Basics.round (Basics.toFloat max * 0.6)

        C ->
            Basics.round (Basics.toFloat max * 0.3)

        D ->
            0

        Invalid ->
            0


newScoreRank : Int -> Int -> Rank
newScoreRank num max =
    if num >= scoreBorder max SSS then
        SSS

    else if num >= scoreBorder max SS then
        SS

    else if num >= scoreBorder max S then
        S

    else if num >= scoreBorder max A then
        A

    else if num >= scoreBorder max B then
        B

    else if num >= scoreBorder max C then
        C

    else if num >= scoreBorder max D then
        D

    else
        Invalid


scoreBorder : Int -> Rank -> Int
scoreBorder max rank =
    case rank of
        SSS ->
            Basics.round (Basics.toFloat max * 0.98)

        SS ->
            Basics.round (Basics.toFloat max * 0.95)

        S ->
            Basics.round (Basics.toFloat max * 0.9)

        A ->
            Basics.round (Basics.toFloat max * 0.8)

        B ->
            Basics.round (Basics.toFloat max * 0.6)

        C ->
            Basics.round (Basics.toFloat max * 0.3)

        D ->
            0

        Invalid ->
            0


toString : Rank -> String
toString rank =
    case rank of
        SSS ->
            "SSS"

        SS ->
            "SS"

        S ->
            "S"

        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        Invalid ->
            "---"


invalid : Rank
invalid =
    Invalid


allRankList : List Rank
allRankList =
    [ SSS, SS, S, A, B, C, D ]
