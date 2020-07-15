module Page.Home.UserPlayRecords.UserPlayRecord exposing
    ( UserPlayRecord
    , new
    , toBestCombo
    , toBestScore
    , toPlayCount
    )

import OwnRecord exposing (OwnRecordDto)


type UserPlayRecord
    = UserPlayRecord
        { bestCombo : Maybe Int
        , bestScore : Maybe Int
        , playCount : Int
        }


toBestCombo : UserPlayRecord -> Maybe Int
toBestCombo (UserPlayRecord { bestCombo }) =
    bestCombo


toBestScore : UserPlayRecord -> Maybe Int
toBestScore (UserPlayRecord { bestScore }) =
    bestScore


toPlayCount : UserPlayRecord -> Int
toPlayCount (UserPlayRecord { playCount }) =
    playCount


new : OwnRecordDto -> UserPlayRecord
new ownRecordDto =
    UserPlayRecord
        { bestCombo =
            ownRecordDto.playRecord
                |> Maybe.map .bestCombo
        , bestScore =
            ownRecordDto.playRecord
                |> Maybe.map .bestScore
        , playCount =
            ownRecordDto.playRecord
                |> Maybe.map .playCount
                |> Maybe.withDefault 0
        }
