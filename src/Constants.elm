module Constants exposing
    ( allKeyStr
    , goodRange
    , goodScore
    , greatRange
    , greatScore
    , longScore
    , longTimeDuration
    , longTimeOffset
    , missRange
    , perfectRange
    , perfectScore
    )

import Page.Play.KeyStr exposing (KeyStr)


allKeyStr : List KeyStr
allKeyStr =
    [ "S", "D", "F", "J", "K", "L" ]


longTimeOffset : Float
longTimeOffset =
    150


longTimeDuration : Float
longTimeDuration =
    200


perfectRange : Float
perfectRange =
    50


greatRange : Float
greatRange =
    90


goodRange : Float
goodRange =
    130


missRange : Float
missRange =
    200


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
