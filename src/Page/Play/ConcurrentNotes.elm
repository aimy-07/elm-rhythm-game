module Page.Play.ConcurrentNotes exposing (ConcurrentNotes, new, toJustTime, toNotes, updateNotes, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JustTime exposing (JustTime)
import Page.Play.Note as Note exposing (Note)
import Page.Play.Speed exposing (Speed)


type ConcurrentNotes
    = ConcurrentNotes
        { justTime : JustTime
        , notes : List Note
        }


new : JustTime -> List Note -> ConcurrentNotes
new justTime notes =
    ConcurrentNotes
        { justTime = justTime
        , notes = notes
        }


toJustTime : ConcurrentNotes -> JustTime
toJustTime (ConcurrentNotes { justTime }) =
    justTime


toNotes : ConcurrentNotes -> List Note
toNotes (ConcurrentNotes { notes }) =
    notes


updateNotes : List Note -> ConcurrentNotes -> ConcurrentNotes
updateNotes notes (ConcurrentNotes concurrentNotes) =
    ConcurrentNotes { concurrentNotes | notes = notes }


view : CurrentMusicTime -> Speed -> ConcurrentNotes -> Html msg
view currentMusicTime speed (ConcurrentNotes { justTime, notes }) =
    div []
        (notes
            |> List.map
                (\note ->
                    Note.view currentMusicTime justTime speed note
                )
        )
