module Page.Play.MusicInfo exposing (MusicInfo, MusicInfoDto, create, init, isLoaded, toAllNotes, toBpm, toFullTime, toStringBpm, updateNotesKeyDown, updateNotesOverMiss)

import Page.Play.ConcurrentNotes as ConcurrentNotes exposing (ConcurrentNotes)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.Note as Note exposing (Note)


type MusicInfo
    = NotLoaded
    | Loaded
        { allNotes : List ConcurrentNotes
        , maxCombo : MaxCombo
        , fullTime : FullTime
        , bpm : Bpm
        }


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
        Loaded _ ->
            True

        NotLoaded ->
            False


toAllNotes : MusicInfo -> List ConcurrentNotes
toAllNotes musicInfo =
    case musicInfo of
        Loaded { allNotes } ->
            allNotes

        NotLoaded ->
            []


toMaxCombo : MusicInfo -> MaxCombo
toMaxCombo musicInfo =
    case musicInfo of
        Loaded { maxCombo } ->
            maxCombo

        NotLoaded ->
            0


toFullTime : MusicInfo -> FullTime
toFullTime musicInfo =
    case musicInfo of
        Loaded { fullTime } ->
            fullTime

        NotLoaded ->
            0


toStringFullTime : MusicInfo -> String
toStringFullTime musicInfo =
    case musicInfo of
        Loaded { fullTime } ->
            String.fromFloat fullTime

        NotLoaded ->
            "0"


toBpm : MusicInfo -> Bpm
toBpm musicInfo =
    case musicInfo of
        Loaded { bpm } ->
            bpm

        NotLoaded ->
            0


toStringBpm : MusicInfo -> String
toStringBpm musicInfo =
    case musicInfo of
        Loaded { bpm } ->
            String.fromInt bpm

        NotLoaded ->
            "0"


updateNotesOverMiss : MusicInfo -> MusicInfo
updateNotesOverMiss musicInfo =
    case musicInfo of
        Loaded musicInfo_ ->
            case musicInfo_.allNotes of
                head :: tails ->
                    Loaded { musicInfo_ | allNotes = tails }

                [] ->
                    musicInfo

        NotLoaded ->
            musicInfo


updateNotesKeyDown : LinePosition -> MusicInfo -> MusicInfo
updateNotesKeyDown position musicInfo =
    case musicInfo of
        Loaded musicInfo_ ->
            case musicInfo_.allNotes of
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
                        Loaded { musicInfo_ | allNotes = tails }

                    else
                        let
                            nextHead =
                                ConcurrentNotes.updateNotes nextHeadNotes head
                        in
                        Loaded { musicInfo_ | allNotes = nextHead :: tails }

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
                    (\rawConcurrentNote ->
                        let
                            notes =
                                rawConcurrentNote.notes
                                    |> List.indexedMap
                                        (\index num ->
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
                                            if num < 0 then
                                                Nothing

                                            else
                                                Just (Note.new position num)
                                        )
                                    |> List.filterMap (\maybeNotes -> maybeNotes)
                        in
                        ConcurrentNotes.new rawConcurrentNote.justTime notes
                    )
    in
    Loaded
        { allNotes = allNotes
        , maxCombo = List.length allNotes
        , fullTime = fullTime
        , bpm = bpm
        }


type alias MusicInfoDto =
    { fullTime : Float
    , bpm : Float
    , allNotes : List NoteDto
    }


type alias NoteDto =
    { justTime : Float
    , notes : List Float
    }
