module Page.Play.Note exposing (EndTime, LongTime, Note, isLongNote, isSamePosition, isSingleNote, newLongNote, newSingleNote, toLongTime, toPosition, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JustTime exposing (JustTime)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.Speed exposing (Speed)


type Note
    = Single
        { position : LinePosition
        }
    | Long
        { position : LinePosition
        , longTime : Float
        }


type alias LongTime =
    Float


type alias EndTime =
    Float


newSingleNote : String -> Note
newSingleNote keyStr =
    Single
        { position = LinePosition.new keyStr
        }


newLongNote : String -> LongTime -> Note
newLongNote keyStr longTime =
    Long
        { position = LinePosition.new keyStr
        , longTime = longTime
        }


isSingleNote : Note -> Bool
isSingleNote note =
    case note of
        Single _ ->
            True

        Long _ ->
            False


isLongNote : Note -> Bool
isLongNote note =
    case note of
        Single _ ->
            False

        Long _ ->
            True


toPosition : Note -> LinePosition
toPosition note =
    case note of
        Single { position } ->
            position

        Long { position } ->
            position


toLongTime : Note -> LongTime
toLongTime note =
    case note of
        Single _ ->
            0

        Long { longTime } ->
            longTime


isSamePosition : LinePosition -> Note -> Bool
isSamePosition linePosition note =
    LinePosition.isSamePosition linePosition (toPosition note)


view : Note -> CurrentMusicTime -> JustTime -> Speed -> Html msg
view note currentMusicTime justTime speed =
    let
        bottom =
            (justTime - currentMusicTime) * speed
    in
    case note of
        Single { position } ->
            div
                [ class "note"
                , style "bottom" (String.fromFloat bottom ++ "px")
                , style "left" (LinePosition.styleLeft <| position)
                ]
                []

        Long { position, longTime } ->
            let
                height =
                    longTime * speed
            in
            div
                [ class "note"
                , class "long"
                , style "height" (String.fromFloat height ++ "px")
                , style "bottom" (String.fromFloat bottom ++ "px")
                , style "left" (LinePosition.styleLeft <| position)
                ]
                []
