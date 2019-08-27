module Page.Play.LongNoteLine exposing (EndTime, LongNoteLine, getEndTime, getTimeCounter, new, toEndTime, toPosition, toTimeCounter, updateTimeCounter, view, viewLongNoteEffect)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.ConcurrentNotes as ConcurrentNotes exposing (ConcurrentNotes)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JustTime exposing (JustTime)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.Note as Note exposing (Note)
import Page.Play.Speed exposing (Speed)


type LongNoteLine
    = LongNoteLine
        { position : LinePosition
        , endTime : EndTime
        , timeCounter : Int
        }


type alias EndTime =
    Float


new : LinePosition -> EndTime -> Int -> LongNoteLine
new position endTime timeCounter =
    LongNoteLine
        { position = position
        , endTime = endTime
        , timeCounter = timeCounter
        }


toPosition : LongNoteLine -> LinePosition
toPosition (LongNoteLine { position }) =
    position


toEndTime : LongNoteLine -> EndTime
toEndTime (LongNoteLine { endTime }) =
    endTime


toTimeCounter : LongNoteLine -> Int
toTimeCounter (LongNoteLine { timeCounter }) =
    timeCounter


updateTimeCounter : LongNoteLine -> LongNoteLine
updateTimeCounter (LongNoteLine longNoteLine) =
    LongNoteLine { longNoteLine | timeCounter = longNoteLine.timeCounter - 10 }


getEndTime : LinePosition -> Maybe ConcurrentNotes -> EndTime
getEndTime position maybeHead =
    maybeHead
        |> Maybe.map
            (\head ->
                let
                    justTime =
                        ConcurrentNotes.toJustTime head

                    notes =
                        ConcurrentNotes.toNotes head
                in
                notes
                    |> List.filter (\note -> Note.toPosition note == position)
                    |> List.head
                    |> Maybe.map (\note -> justTime + Note.toLongTime note)
                    |> Maybe.withDefault 0
            )
        |> Maybe.withDefault 0


getTimeCounter : LinePosition -> Maybe ConcurrentNotes -> Int
getTimeCounter position maybeHead =
    maybeHead
        |> Maybe.map
            (\head ->
                let
                    justTime =
                        ConcurrentNotes.toJustTime head

                    notes =
                        ConcurrentNotes.toNotes head
                in
                notes
                    |> List.filter (\note -> Note.toPosition note == position)
                    |> List.head
                    |> Maybe.map
                        (\note ->
                            Note.toLongTime note
                                / 100
                                |> Basics.floor
                                |> (*) 100
                        )
                    |> Maybe.withDefault -1
            )
        |> Maybe.withDefault -1


view : CurrentMusicTime -> Speed -> LongNoteLine -> Html msg
view currentMusicTime speed (LongNoteLine { position, endTime }) =
    let
        height =
            (endTime - currentMusicTime) * speed
    in
    div
        [ class "play_note_longLine"
        , style "height" (String.fromFloat height ++ "px")
        , style "bottom" "0px"
        , style "left" (LinePosition.styleLeft position)
        ]
        []


viewLongNoteEffect : LongNoteLine -> List LongNoteLine -> Html msg
viewLongNoteEffect (LongNoteLine { position }) longNoteLines =
    let
        isPressing =
            longNoteLines
                |> List.any
                    (\longNoteLine ->
                        toPosition longNoteLine == position
                    )
    in
    if isPressing then
        div
            [ class "play_longNoteEffect"
            , style "left" (LinePosition.styleLeft position)
            ]
            []

    else
        text ""
