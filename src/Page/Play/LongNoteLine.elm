module Page.Play.LongNoteLine exposing (EndTime, LongNoteLine, getEndTime, new, toEndTime, toPosition, view)

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
        }


type alias EndTime =
    Float


new : LinePosition -> EndTime -> LongNoteLine
new position endTime =
    LongNoteLine
        { position = position
        , endTime = endTime
        }


toPosition : LongNoteLine -> LinePosition
toPosition (LongNoteLine { position }) =
    position


toEndTime : LongNoteLine -> EndTime
toEndTime (LongNoteLine { endTime }) =
    endTime


getEndTime : CurrentMusicTime -> LinePosition -> Maybe ConcurrentNotes -> EndTime
getEndTime currentMusicTime position maybeHead =
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
