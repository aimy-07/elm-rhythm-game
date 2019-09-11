module Page.Play.Note exposing
    ( JustTime
    , LongTime
    , Note
    , NoteDto
    , isLong
    , new
    , toJustTime
    , toLongTime
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Speed exposing (Speed)


type Note
    = Note
        { justTime : JustTime
        , longTime : LongTime
        }


type alias JustTime =
    Float


type alias LongTime =
    Float


type alias NoteDto =
    { justTime : Float
    , longTime : Float
    }


new : NoteDto -> Note
new noteDto =
    Note
        { justTime = noteDto.justTime
        , longTime = noteDto.longTime
        }


toJustTime : Note -> JustTime
toJustTime (Note { justTime }) =
    justTime


toLongTime : Note -> LongTime
toLongTime (Note { longTime }) =
    longTime


isLong : Note -> Bool
isLong (Note { longTime }) =
    longTime > 0


view : CurrentMusicTime -> Speed -> Note -> Html msg
view currentMusicTime speed (Note { justTime, longTime }) =
    let
        bottom =
            (justTime - currentMusicTime) * speed

        height =
            longTime * speed

        styleClass =
            if longTime == 0 then
                class "playNote_note"

            else
                class "playNote_note long"
    in
    div []
        [ div
            [ styleClass
            , style "bottom" (String.fromFloat (bottom - 20) ++ "px")
            ]
            []
        , div
            [ class "playNote_longLine"
            , style "bottom" (String.fromFloat bottom ++ "px")
            , style "height" (String.fromFloat height ++ "px")
            ]
            []
        ]
