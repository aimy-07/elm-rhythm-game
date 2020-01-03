module Rank exposing
    ( Rank
    , allRankList
    , border
    , invalid
    , new
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


new : Int -> Int -> Rank
new num max =
    if num >= border max SSS then
        SSS

    else if num >= border max SS then
        SS

    else if num >= border max S then
        S

    else if num >= border max A then
        A

    else if num >= border max B then
        B

    else if num >= border max C then
        C

    else if num >= border max D then
        D

    else
        Invalid


border : Int -> Rank -> Int
border max rank =
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
            Basics.round (Basics.toFloat max * 0.7)

        C ->
            Basics.round (Basics.toFloat max * 0.5)

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
