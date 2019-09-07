module Page.Play.AllNotes exposing
    ( AllNotes
    , AllNotesDto
    , hasOverMissNotes
    , init
    , map
    , new
    , toNotesPerLane
    , updateLongNoteLines
    , updateNotesKeyDown
    , updateNotesKeyUp
    , updateNotesOverMiss
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.KeyStr as KeyStr exposing (KeyStr)
import Page.Play.Note as Note exposing (Note, NoteDto)
import Page.Play.NotesPerLane as NotesPerLane exposing (NotesPerLane)
import Page.Play.Speed exposing (Speed)


type AllNotes
    = AllNotes (List NotesPerLane)


type alias AllNotesDto =
    { laneS : List NoteDto
    , laneD : List NoteDto
    , laneF : List NoteDto
    , laneJ : List NoteDto
    , laneK : List NoteDto
    , laneL : List NoteDto
    }


new : AllNotesDto -> AllNotes
new allNotes =
    AllNotes
        (KeyStr.allKeyStr
            |> List.map
                (\keyStr ->
                    case keyStr of
                        "S" ->
                            NotesPerLane.new "S" allNotes.laneS

                        "D" ->
                            NotesPerLane.new "D" allNotes.laneD

                        "F" ->
                            NotesPerLane.new "F" allNotes.laneF

                        "J" ->
                            NotesPerLane.new "J" allNotes.laneJ

                        "K" ->
                            NotesPerLane.new "K" allNotes.laneK

                        "L" ->
                            NotesPerLane.new "L" allNotes.laneL

                        _ ->
                            NotesPerLane.invalid
                )
        )


init : AllNotes
init =
    AllNotes []


map : (NotesPerLane -> a) -> AllNotes -> List a
map func (AllNotes allNotes) =
    List.map func allNotes


toNotesPerLane : KeyStr -> AllNotes -> NotesPerLane
toNotesPerLane keyStr (AllNotes allNotes) =
    allNotes
        |> List.filter
            (\notesPerLane -> NotesPerLane.toKeyStr notesPerLane == keyStr)
        |> List.head
        |> Maybe.withDefault NotesPerLane.invalid


updateNotesKeyDown : KeyStr -> AllNotes -> AllNotes
updateNotesKeyDown keyStr (AllNotes allNotes) =
    AllNotes
        (allNotes
            |> List.map
                (\notesPerLane ->
                    if NotesPerLane.toKeyStr notesPerLane == keyStr then
                        NotesPerLane.updateKeyDown notesPerLane

                    else
                        notesPerLane
                )
        )


updateNotesKeyUp : KeyStr -> AllNotes -> AllNotes
updateNotesKeyUp keyStr (AllNotes allNotes) =
    AllNotes
        (allNotes
            |> List.map
                (\notesPerLane ->
                    if NotesPerLane.toKeyStr notesPerLane == keyStr then
                        NotesPerLane.updateKeyUp notesPerLane

                    else
                        notesPerLane
                )
        )


hasOverMissNotes : CurrentMusicTime -> AllNotes -> Bool
hasOverMissNotes currentMusicTime (AllNotes allNotes) =
    allNotes
        |> List.any
            (\notesPerLane ->
                NotesPerLane.isOverMiss currentMusicTime notesPerLane
            )


updateNotesOverMiss : CurrentMusicTime -> AllNotes -> AllNotes
updateNotesOverMiss currentMusicTime (AllNotes allNotes) =
    AllNotes
        (allNotes
            |> List.map
                (\notesPerLane ->
                    let
                        isOverMiss =
                            NotesPerLane.isOverMiss currentMusicTime notesPerLane
                    in
                    if isOverMiss then
                        NotesPerLane.updateOverMiss currentMusicTime notesPerLane

                    else
                        notesPerLane
                )
        )


updateLongNoteLines : CurrentMusicTime -> AllNotes -> AllNotes
updateLongNoteLines currentMusicTime (AllNotes allNotes) =
    AllNotes
        (allNotes
            |> List.map
                (\notesPerLane ->
                    NotesPerLane.updateLongNoteLine currentMusicTime notesPerLane
                )
        )


view : CurrentMusicTime -> Speed -> AllNotes -> Html msg
view currentMusicTime speed (AllNotes allNotes) =
    div [ class "play_centerLine", id "judge_area" ]
        (allNotes
            |> List.map
                (\notesPerLane ->
                    NotesPerLane.view currentMusicTime speed notesPerLane
                )
        )
