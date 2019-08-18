module Page.Play.MusicInfo exposing (MusicInfo, MusicInfoDto, create, init, isLoaded, toAllNotes, toBpm, toFullTime, toStringBpm, updateNotesKeyDown, updateNotesOverMiss)

import Page.Play.ConcurrentNotes as ConcurrentNotes exposing (ConcurrentNotes)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.Note as Note exposing (Note)


type MusicInfo
    = NotLoaded
    | Loaded (List ConcurrentNotes) MaxCombo FullTime Bpm


type alias MaxCombo =
    Int


type alias FullTime =
    Float


type alias Bpm =
    Int


init : MusicInfo
init =
    NotLoaded


isLoaded : MusicInfo -> Bool
isLoaded musicInfo =
    case musicInfo of
        Loaded _ _ _ _ ->
            True

        NotLoaded ->
            False


toAllNotes : MusicInfo -> List ConcurrentNotes
toAllNotes musicInfo =
    case musicInfo of
        Loaded allNotes _ _ _ ->
            allNotes

        NotLoaded ->
            []


toFullTime : MusicInfo -> FullTime
toFullTime musicInfo =
    case musicInfo of
        Loaded _ _ fillTime _ ->
            fillTime

        NotLoaded ->
            0


toStringFullTime : MusicInfo -> String
toStringFullTime musicInfo =
    case musicInfo of
        Loaded _ _ fillTime _ ->
            String.fromFloat fillTime

        NotLoaded ->
            "0"


toBpm : MusicInfo -> Bpm
toBpm musicInfo =
    case musicInfo of
        Loaded _ _ _ bpm ->
            bpm

        NotLoaded ->
            0


toStringBpm : MusicInfo -> String
toStringBpm musicInfo =
    case musicInfo of
        Loaded _ _ _ bpm ->
            String.fromInt bpm

        NotLoaded ->
            "0"


updateNotesOverMiss : MusicInfo -> MusicInfo
updateNotesOverMiss musicInfo =
    case musicInfo of
        Loaded allNotes maxCombo fullTime bpm ->
            case allNotes of
                head :: tails ->
                    Loaded tails maxCombo fullTime bpm

                [] ->
                    musicInfo

        NotLoaded ->
            musicInfo


updateNotesKeyDown : LinePosition -> MusicInfo -> MusicInfo
updateNotesKeyDown position musicInfo =
    case musicInfo of
        Loaded allNotes maxCombo fullTime bpm ->
            case allNotes of
                head :: tails ->
                    let
                        justTime =
                            ConcurrentNotes.toJustTime head

                        notes =
                            ConcurrentNotes.toNotes head

                        nextHeadNotes =
                            notes
                                |> List.filter
                                    (\note -> not <| Note.isSamePosition position note)
                    in
                    if nextHeadNotes == [] then
                        Loaded tails maxCombo fullTime bpm

                    else
                        let
                            nextHead =
                                ConcurrentNotes.updateNotes nextHeadNotes head
                        in
                        Loaded (nextHead :: tails) maxCombo fullTime bpm

                [] ->
                    musicInfo

        NotLoaded ->
            musicInfo


create : MusicInfoDto -> MusicInfo
create rawMusicInfo =
    let
        fullTime =
            rawMusicInfo.fullTime

        bpm =
            Basics.round rawMusicInfo.bpm

        allNotes =
            rawMusicInfo.allNotes
                |> List.map
                    (\concurrentNote ->
                        let
                            notes =
                                concurrentNote.notes
                                    |> List.indexedMap
                                        (\index ->
                                            \note ->
                                                let
                                                    position =
                                                        case index of
                                                            0 ->
                                                                "S"

                                                            1 ->
                                                                "D"

                                                            2 ->
                                                                "F"

                                                            3 ->
                                                                "J"

                                                            4 ->
                                                                "K"

                                                            5 ->
                                                                "L"

                                                            _ ->
                                                                Debug.todo "" "Invalid Position"
                                                in
                                                if note < 0 then
                                                    Nothing

                                                else if note == 0 then
                                                    Just (Note.newSingleNote position)

                                                else
                                                    Just (Note.newLongNote position note)
                                        )
                                    |> List.filterMap (\maybeNotes -> maybeNotes)
                        in
                        ConcurrentNotes.new concurrentNote.justTime notes
                    )
    in
    Loaded allNotes (List.length allNotes) fullTime bpm


type alias MusicInfoDto =
    { fullTime : Float
    , bpm : Float
    , allNotes : List NoteDto
    }


type alias NoteDto =
    { justTime : Float
    , notes : List Float
    }
