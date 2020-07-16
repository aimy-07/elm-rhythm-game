module Page.Play.Guideline exposing (Guideline, createGuidelines, update, view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (viewIf)


type Guideline
    = Guideline
        { justTime : JustTime
        , isPassed : Bool
        }


type alias JustTime =
    Float


createGuidelines :
    { bpm : Int
    , beatsCountPerMeasure : Int
    , offset : Float
    , fullTime : Float
    }
    -> List Guideline
createGuidelines { bpm, beatsCountPerMeasure, offset, fullTime } =
    let
        timePerBeat =
            60 * 1000 / Basics.toFloat bpm

        timePerMeasure =
            timePerBeat * Basics.toFloat beatsCountPerMeasure

        maxMeasure =
            (Basics.floor <| fullTime * 1000 / timePerMeasure) - 1
    in
    List.range 0 maxMeasure
        |> List.map
            (\measure ->
                let
                    justTime =
                        (Basics.toFloat measure * Basics.toFloat beatsCountPerMeasure * timePerBeat) + offset * 1000
                in
                Guideline
                    { justTime = justTime
                    , isPassed = False
                    }
            )


update : CurrentMusicTime -> Guideline -> Guideline
update currentMusicTime (Guideline guideline) =
    if guideline.justTime < currentMusicTime then
        Guideline { guideline | isPassed = True }

    else
        Guideline guideline


view : CurrentMusicTime -> NotesSpeed -> Guideline -> Html msg
view currentMusicTime notesSpeed (Guideline guideline) =
    let
        bottom =
            (guideline.justTime - currentMusicTime) * notesSpeed
    in
    div
        [ class "playGuideline_line"
        , style "transform" ("translateY(" ++ String.fromFloat -(bottom + 4) ++ "px)")
        ]
        []
        |> viewIf (not guideline.isPassed)
