module Page.Play.MusicInfo exposing
    ( MusicInfo
    , MusicInfoDto
    , create
    , init
    , isLoaded
    , toAllNotes
    , toBpm
    , toFullTime
    , toStringBpm
    , toStringMaxCombo
    )

import Page.Play.AllNotes as AllNotes exposing (AllNotes)
import Page.Play.Note as Note exposing (NoteDto)


type MusicInfo
    = NotLoaded
    | Loaded
        { allNotes : AllNotes
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


toAllNotes : MusicInfo -> AllNotes
toAllNotes musicInfo =
    case musicInfo of
        Loaded { allNotes } ->
            allNotes

        NotLoaded ->
            AllNotes.init


toMaxCombo : MusicInfo -> MaxCombo
toMaxCombo musicInfo =
    case musicInfo of
        Loaded { maxCombo } ->
            maxCombo

        NotLoaded ->
            0


toStringMaxCombo : MusicInfo -> String
toStringMaxCombo musicInfo =
    case musicInfo of
        Loaded { maxCombo } ->
            String.fromInt maxCombo

        NotLoaded ->
            "0"


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


create : MusicInfoDto -> MusicInfo
create rawMusicInfo =
    let
        fullTime =
            rawMusicInfo.fullTime

        bpm =
            Basics.round rawMusicInfo.bpm

        maxCombo =
            rawMusicInfo.maxCombo

        allNotes =
            AllNotes.new rawMusicInfo.allNotes
    in
    Loaded
        { allNotes = allNotes
        , maxCombo = maxCombo
        , fullTime = fullTime
        , bpm = bpm
        }


type alias MusicInfoDto =
    { fullTime : Float
    , bpm : Float
    , maxCombo : Int
    , allNotes :
        { laneS : List NoteDto
        , laneD : List NoteDto
        , laneF : List NoteDto
        , laneJ : List NoteDto
        , laneK : List NoteDto
        , laneL : List NoteDto
        }
    }
