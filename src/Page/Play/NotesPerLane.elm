module Page.Play.NotesPerLane exposing
    ( NotesPerLane
    , invalid
    , isOverMiss
    , maybeHeadNote
    , new
    , toKeyStr
    , toMaybeLongNoteLine
    , toNotes
    , toPx
    , updateKeyDown
    , updateKeyUp
    , updateLongNoteLine
    , updateOverMiss
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.KeyStr as KeyStr exposing (KeyStr)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.LongNoteLine as LongNoteLine exposing (EndTime, LongNoteLine)
import Page.Play.Note as Note exposing (Note, NoteDto)
import Page.Play.Speed exposing (Speed)


type NotesPerLane
    = NotesPerLane
        { keyStr : KeyStr
        , notes : List Note
        , maybeLongNoteLine : Maybe LongNoteLine
        , left : Int
        }


new : KeyStr -> List NoteDto -> NotesPerLane
new keyStr noteDtos =
    NotesPerLane
        { keyStr = keyStr
        , notes =
            noteDtos
                |> List.filterMap
                    (\noteDto ->
                        if noteDto.longTime >= 0 then
                            Just (Note.new noteDto)

                        else
                            Nothing
                    )
        , maybeLongNoteLine = Nothing
        , left = Lane.leftFromKeyStr keyStr
        }


invalid : NotesPerLane
invalid =
    NotesPerLane
        { keyStr = ""
        , notes = []
        , maybeLongNoteLine = Nothing
        , left = -1000
        }


toKeyStr : NotesPerLane -> KeyStr
toKeyStr (NotesPerLane { keyStr }) =
    keyStr


toNotes : NotesPerLane -> List Note
toNotes (NotesPerLane { notes }) =
    notes


toMaybeLongNoteLine : NotesPerLane -> Maybe LongNoteLine
toMaybeLongNoteLine (NotesPerLane { maybeLongNoteLine }) =
    maybeLongNoteLine


toPx : NotesPerLane -> KeyStr
toPx (NotesPerLane { left }) =
    String.fromInt left ++ "px"


maybeHeadNote : NotesPerLane -> Maybe Note
maybeHeadNote (NotesPerLane { notes, maybeLongNoteLine }) =
    case maybeLongNoteLine of
        -- ロングノートがあるときはシングルノートの判定はできない
        Just _ ->
            Nothing

        Nothing ->
            List.head notes


updateKeyDown : NotesPerLane -> NotesPerLane
updateKeyDown (NotesPerLane notesPerLane) =
    maybeHeadNote (NotesPerLane notesPerLane)
        |> Maybe.map
            (\note ->
                -- ロングノート
                if Note.toLongTime note > 0 then
                    NotesPerLane
                        { notesPerLane
                            | notes = List.drop 1 notesPerLane.notes
                            , maybeLongNoteLine = Just (LongNoteLine.new note)
                        }
                    -- シングルノート

                else
                    NotesPerLane
                        { notesPerLane
                            | notes = List.drop 1 notesPerLane.notes
                            , maybeLongNoteLine = Nothing
                        }
            )
        |> Maybe.withDefault (NotesPerLane notesPerLane)


updateKeyUp : NotesPerLane -> NotesPerLane
updateKeyUp (NotesPerLane notesPerLane) =
    NotesPerLane { notesPerLane | maybeLongNoteLine = Nothing }


isOverMiss : CurrentMusicTime -> NotesPerLane -> Bool
isOverMiss currentMusicTime (NotesPerLane notesPerLane) =
    maybeHeadNote (NotesPerLane notesPerLane)
        |> JudgeKind.isOverMiss currentMusicTime


updateOverMiss : CurrentMusicTime -> NotesPerLane -> NotesPerLane
updateOverMiss currentMusicTime (NotesPerLane notesPerLane) =
    maybeHeadNote (NotesPerLane notesPerLane)
        |> Maybe.map
            (\_ ->
                NotesPerLane
                    { notesPerLane
                        | notes = List.drop 1 notesPerLane.notes
                        , maybeLongNoteLine = Nothing
                    }
            )
        |> Maybe.withDefault (NotesPerLane notesPerLane)


updateLongNoteLine : CurrentMusicTime -> NotesPerLane -> NotesPerLane
updateLongNoteLine currentMusicTime (NotesPerLane notesPerLane) =
    let
        nextMaybeLongNoteLine =
            notesPerLane.maybeLongNoteLine
                |> Maybe.andThen
                    (\longNoteLine ->
                        if LongNoteLine.toEndTime longNoteLine > currentMusicTime then
                            Just (LongNoteLine.updateTimeCounter longNoteLine)

                        else
                            Nothing
                    )
    in
    NotesPerLane { notesPerLane | maybeLongNoteLine = nextMaybeLongNoteLine }


view : CurrentMusicTime -> Speed -> NotesPerLane -> Html msg
view currentMusicTime speed (NotesPerLane { notes, maybeLongNoteLine, left }) =
    div
        [ style "left" (String.fromInt left ++ "px")
        , style "position" "absolute"
        ]
        [ div
            []
            (List.map (\note -> Note.view currentMusicTime speed note) notes)
        , LongNoteLine.view currentMusicTime speed maybeLongNoteLine
        ]
