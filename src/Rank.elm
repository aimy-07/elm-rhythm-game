module Rank exposing
    ( Rank
    , allRankList
    , boundaryCombo
    , boundaryScore
    , newComboRank
    , newScoreRank
    , toString
    )


type Rank
    = Rank String


toString : Rank -> String
toString (Rank rank) =
    rank


allRankList : List Rank
allRankList =
    [ Rank "SSS"
    , Rank "SS"
    , Rank "S"
    , Rank "A"
    , Rank "B"
    , Rank "C"
    , Rank "D"
    ]


newScoreRank : Int -> Int -> Rank
newScoreRank score maxScore =
    Rank
        (if score >= boundaryScore maxScore (Rank "SSS") then
            "SSS"

         else if score >= boundaryScore maxScore (Rank "SS") then
            "SS"

         else if score >= boundaryScore maxScore (Rank "S") then
            "S"

         else if score >= boundaryScore maxScore (Rank "A") then
            "A"

         else if score >= boundaryScore maxScore (Rank "B") then
            "B"

         else if score >= boundaryScore maxScore (Rank "C") then
            "C"

         else if score >= boundaryScore maxScore (Rank "D") then
            "D"

         else
            ""
        )


boundaryScore : Int -> Rank -> Int
boundaryScore maxScore (Rank rank) =
    case rank of
        "SSS" ->
            maxScore

        "SS" ->
            Basics.round (Basics.toFloat maxScore * 0.95)

        "S" ->
            Basics.round (Basics.toFloat maxScore * 0.9)

        "A" ->
            Basics.round (Basics.toFloat maxScore * 0.8)

        "B" ->
            Basics.round (Basics.toFloat maxScore * 0.7)

        "C" ->
            Basics.round (Basics.toFloat maxScore * 0.5)

        "D" ->
            0

        _ ->
            0


newComboRank : Int -> Int -> Rank
newComboRank combo maxCombo =
    Rank
        (if combo >= boundaryScore maxCombo (Rank "SSS") then
            "SSS"

         else if combo >= boundaryScore maxCombo (Rank "SS") then
            "SS"

         else if combo >= boundaryScore maxCombo (Rank "S") then
            "S"

         else if combo >= boundaryScore maxCombo (Rank "A") then
            "A"

         else if combo >= boundaryScore maxCombo (Rank "B") then
            "B"

         else if combo >= boundaryScore maxCombo (Rank "C") then
            "C"

         else if combo >= boundaryScore maxCombo (Rank "D") then
            "D"

         else
            ""
        )


boundaryCombo : Int -> Rank -> Int
boundaryCombo maxCombo (Rank rank) =
    case rank of
        "SSS" ->
            maxCombo

        "SS" ->
            Basics.round (Basics.toFloat maxCombo * 0.95)

        "S" ->
            Basics.round (Basics.toFloat maxCombo * 0.9)

        "A" ->
            Basics.round (Basics.toFloat maxCombo * 0.8)

        "B" ->
            Basics.round (Basics.toFloat maxCombo * 0.7)

        "C" ->
            Basics.round (Basics.toFloat maxCombo * 0.5)

        "D" ->
            0

        _ ->
            0
