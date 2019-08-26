module Page.Play.Note exposing (LongTime, Note, isSamePosition, new, toLongTime, toPosition, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JustTime exposing (JustTime)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.Speed exposing (Speed)


type Note
    = Note
        { position : LinePosition
        , longTime : LongTime
        }


type alias LongTime =
    Float


new : String -> LongTime -> Note
new keyStr longTime =
    Note
        { position = LinePosition.new keyStr
        , longTime = longTime
        }


toPosition : Note -> LinePosition
toPosition (Note { position }) =
    position


toLongTime : Note -> LongTime
toLongTime (Note { longTime }) =
    longTime


isSamePosition : LinePosition -> Note -> Bool
isSamePosition linePosition note =
    LinePosition.isSamePosition linePosition (toPosition note)


view : CurrentMusicTime -> JustTime -> Speed -> Note -> Html msg
view currentMusicTime justTime speed (Note { position, longTime }) =
    let
        bottom =
            (justTime - currentMusicTime) * speed

        height =
            longTime * speed

        styleClass =
            if longTime == 0 then
                class "play_note"

            else
                class "play_note long"
    in
    div []
        [ div
            [ styleClass
            , style "bottom" (String.fromFloat (bottom - 20) ++ "px")
            , style "left" (LinePosition.styleLeft position)
            ]
            []
        , div
            [ class "play_note_longLine"
            , style "bottom" (String.fromFloat (bottom + 0) ++ "px")
            , style "left" (LinePosition.styleLeft position)
            , style "height" (String.fromFloat height ++ "px")
            ]
            []
        ]
