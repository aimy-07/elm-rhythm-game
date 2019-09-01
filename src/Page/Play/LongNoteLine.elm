module Page.Play.LongNoteLine exposing
    ( EndTime
    , LongNoteLine
    , new
    , toEndTime
    , toTimeCounter
    , updateTimeCounter
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Note as Note exposing (JustTime, Note)
import Page.Play.Speed exposing (Speed)


type LongNoteLine
    = LongNoteLine
        { endTime : EndTime
        , timeCounter : Int
        }


type alias EndTime =
    Float


new : Note -> LongNoteLine
new note =
    LongNoteLine
        { endTime = calcEndTime note
        , timeCounter = calcTimeCounter note
        }


toEndTime : LongNoteLine -> EndTime
toEndTime (LongNoteLine { endTime }) =
    endTime


toTimeCounter : LongNoteLine -> Int
toTimeCounter (LongNoteLine { timeCounter }) =
    timeCounter


calcEndTime : Note -> EndTime
calcEndTime note =
    Note.toJustTime note + Note.toLongTime note


calcTimeCounter : Note -> Int
calcTimeCounter note =
    Note.toLongTime note
        / 100
        |> Basics.floor
        |> (*) 100


updateTimeCounter : LongNoteLine -> LongNoteLine
updateTimeCounter (LongNoteLine longNoteLine) =
    LongNoteLine { longNoteLine | timeCounter = longNoteLine.timeCounter - 10 }


view : CurrentMusicTime -> Speed -> Maybe LongNoteLine -> Html msg
view currentMusicTime speed maybeLongNoteLine =
    maybeLongNoteLine
        |> Maybe.map
            (\longNoteLine ->
                let
                    endTime =
                        toEndTime longNoteLine

                    height =
                        (endTime - currentMusicTime) * speed
                in
                div
                    [ class "play_noteLongLine"
                    , style "height" (String.fromFloat height ++ "px")
                    ]
                    []
            )
        |> Maybe.withDefault (text "")
