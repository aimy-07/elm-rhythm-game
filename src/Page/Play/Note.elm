module Page.Play.Note exposing
    ( Note
    , NoteDto
    , headNoteJudge
    , headNoteJudgeEffect
    , isDisabled
    , isLongNote
    , new
    , toJustTime
    , toLongSubNotes
    , update
    , updateKeyDown
    , updateKeyUp
    , view
    )

import Constants exposing (longTimeDuration, longTimeOffset)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge as Judge exposing (Judge(..), JudgeEffect)
import Page.Play.Key exposing (Key)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note.JustTime exposing (JustTime)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (classIf, viewIf)


type Note
    = SingleNote
        { key : Key
        , justTime : JustTime
        , noteStatus : NoteStatus
        }
    | LongNote
        { key : Key
        , justTime : JustTime
        , longTime : LongTime
        , longSubNotes : List LongSubNote
        , noteStatus : NoteStatus
        }


type alias LongTime =
    Float


type alias LongSubNote =
    { justTime : JustTime
    , isJudged : Bool
    }


type NoteStatus
    = NotJudged
    | StartNoteJudged Judge
    | LongJudging Judge
    | Judged
    | OverMissJudged


type alias NoteDto =
    { key : Key
    , justTime : Float
    , longTime : Float
    }


new : NoteDto -> Note
new { key, justTime, longTime } =
    if longTime < longTimeDuration + longTimeOffset then
        SingleNote
            { key = key
            , justTime = justTime
            , noteStatus = NotJudged
            }

    else
        let
            longSubNotes =
                Basics.floor ((longTime - longTimeOffset) / longTimeDuration)
                    |> List.range 1
                    |> List.map
                        (\index ->
                            { justTime = justTime + longTimeDuration * Basics.toFloat index
                            , isJudged = False
                            }
                        )
        in
        LongNote
            { key = key
            , justTime = justTime
            , longTime = longTime
            , longSubNotes = longSubNotes
            , noteStatus = NotJudged
            }


toKey : Note -> Key
toKey note =
    case note of
        SingleNote { key } ->
            key

        LongNote { key } ->
            key


toJustTime : Note -> JustTime
toJustTime note =
    case note of
        SingleNote { justTime } ->
            justTime

        LongNote { justTime } ->
            justTime


toLongTime : Note -> LongTime
toLongTime note =
    case note of
        LongNote { longTime } ->
            longTime

        SingleNote _ ->
            0


toLongSubNotes : Note -> List LongSubNote
toLongSubNotes note =
    case note of
        LongNote { longSubNotes } ->
            longSubNotes

        SingleNote _ ->
            []


toNoteStatus : Note -> NoteStatus
toNoteStatus note =
    case note of
        SingleNote { noteStatus } ->
            noteStatus

        LongNote { noteStatus } ->
            noteStatus


isLongNote : Note -> Bool
isLongNote note =
    case note of
        LongNote _ ->
            True

        SingleNote _ ->
            False


isLongJudging : Note -> Bool
isLongJudging note =
    case toNoteStatus note of
        LongJudging _ ->
            True

        _ ->
            False


isDisabled : Note -> Bool
isDisabled note =
    toNoteStatus note == Judged || toNoteStatus note == OverMissJudged


{-| 毎フレームUpdateごとに、各レーンの先頭のノーツのJudgeを取得する
-}
headNoteJudge : Note -> Judge
headNoteJudge headNote =
    case toNoteStatus headNote of
        NotJudged ->
            Invalid

        StartNoteJudged judge ->
            if judge == Miss then
                Miss

            else
                Invalid

        LongJudging judge ->
            toLongSubNotes headNote
                |> List.filter .isJudged
                |> List.head
                |> Maybe.map (\_ -> judge)
                |> Maybe.withDefault Invalid

        Judged ->
            Invalid

        OverMissJudged ->
            Miss


{-| 毎フレームUpdateごとに、各レーンの先頭のノーツのJudgeEffectを生成する
-}
headNoteJudgeEffect : Note -> JudgeEffect
headNoteJudgeEffect headNote =
    { key = toKey headNote
    , judge = headNoteJudge headNote
    , isLongNote = isLongNote headNote
    }


{-| 毎フレームUpdateごとに、ノーツの状態を更新する
-}
update : CurrentMusicTime -> List Lane -> Note -> Note
update currentMusicTime lanes note =
    case note of
        SingleNote singleNote ->
            case singleNote.noteStatus of
                NotJudged ->
                    if Judge.isOverMiss currentMusicTime (toJustTime note) then
                        -- 判定可能領域内でKeyDownをせず、Missになった
                        SingleNote { singleNote | noteStatus = OverMissJudged }

                    else
                        note

                _ ->
                    note

        LongNote longNote ->
            let
                nextLongSubNotes =
                    toLongSubNotes note
                        |> List.filter (not << .isJudged)
                        |> List.map
                            (\longSubNote ->
                                if Judge.isOverJustTime currentMusicTime longSubNote.justTime then
                                    { longSubNote | isJudged = True }

                                else
                                    longSubNote
                            )

                endTime =
                    toJustTime note + toLongTime note

                isPressing =
                    Lane.pressingKeys (toKey note) lanes
                        |> List.member (toKey note)
            in
            case longNote.noteStatus of
                NotJudged ->
                    if Judge.isOverMiss currentMusicTime (toJustTime note) then
                        -- 判定可能領域内でKeyDownをせず、Missになった
                        LongNote { longNote | longSubNotes = nextLongSubNotes, noteStatus = StartNoteJudged Miss }

                    else
                        note

                StartNoteJudged judge ->
                    if judge == Miss then
                        LongNote { longNote | longSubNotes = nextLongSubNotes, noteStatus = LongJudging Lost }

                    else
                        LongNote { longNote | longSubNotes = nextLongSubNotes, noteStatus = LongJudging judge }

                LongJudging _ ->
                    if endTime < currentMusicTime && List.isEmpty nextLongSubNotes then
                        -- ロングノーツの終わりが判定バーを超えた
                        LongNote { longNote | longSubNotes = nextLongSubNotes, noteStatus = Judged }

                    else if not isPressing then
                        -- ロングノーツ判定中にPauseし、Pause中にKeyUpした
                        LongNote { longNote | longSubNotes = nextLongSubNotes, noteStatus = LongJudging Lost }

                    else
                        -- 長押しを継続している
                        LongNote { longNote | longSubNotes = nextLongSubNotes }

                _ ->
                    note


{-| KeyDown時に、判定結果に応じてノーツの状態を更新する
-}
updateKeyDown : Judge -> Note -> Note
updateKeyDown judge note =
    case note of
        SingleNote singleNote ->
            if judge == Perfect || judge == Nice || judge == Good then
                -- KeyDownでLost or Missになることはない
                SingleNote { singleNote | noteStatus = Judged }

            else
                note

        LongNote longNote ->
            if judge == Perfect || judge == Nice || judge == Good then
                -- KeyDownでLost or Missになることはない
                LongNote { longNote | noteStatus = StartNoteJudged judge }

            else
                note


{-| ロングノーツの終端が判定バーに達する前にKeyUpした時、Lost判定にする
-}
updateKeyUp : Note -> Note
updateKeyUp note =
    case note of
        LongNote longNote ->
            if isLongJudging note then
                LongNote { longNote | noteStatus = LongJudging Lost }

            else
                note

        SingleNote _ ->
            note


view : CurrentMusicTime -> NotesSpeed -> Note -> Html msg
view currentMusicTime notesSpeed note =
    let
        left =
            Lane.leftFromKey <| toKey note

        bottom =
            if isLongJudging note then
                0

            else
                (toJustTime note - currentMusicTime) * notesSpeed

        height =
            if isLongJudging note then
                (toJustTime note + toLongTime note - currentMusicTime) * notesSpeed

            else
                toLongTime note * notesSpeed
    in
    case note of
        SingleNote _ ->
            div
                [ style "left" (String.fromInt left ++ "px")
                , style "position" "absolute"
                ]
                [ div
                    [ class "playNote_note"
                    , style "transform" ("translateY(" ++ String.fromFloat -(bottom + 20) ++ "px) rotate(45deg)")
                    ]
                    []
                ]

        LongNote { noteStatus } ->
            let
                isLongDisabled =
                    noteStatus == LongJudging Lost
            in
            div
                [ style "left" (String.fromInt left ++ "px")
                , style "position" "absolute"
                ]
                [ div
                    [ class "playNote_note long"
                    , style "transform" ("translateY(" ++ String.fromFloat -(bottom + 20) ++ "px) rotate(45deg)")
                    ]
                    []
                    |> viewIf (not <| isLongJudging note)
                , div
                    [ class "playNote_longLine"
                    , classIf isLongDisabled "is-disabled"
                    , style "transform" ("translateY(" ++ String.fromFloat -(bottom + height) ++ "px)")
                    , style "height" (String.fromFloat height ++ "px")
                    ]
                    []

                -- デバッグ用にロングノーツのサブノーツを見える化する時はコメントアウトを外す
                -- , div
                --     []
                --     (toLongSubNotes note
                --         |> List.map (viewLongSubNote currentMusicTime notesSpeed)
                --     )
                ]


{-|

    デバッグ用にロングノーツのサブノーツを見える化したView

-}
viewLongSubNote : CurrentMusicTime -> NotesSpeed -> LongSubNote -> Html msg
viewLongSubNote currentMusicTime notesSpeed longSubNote =
    let
        bottom =
            (longSubNote.justTime - currentMusicTime) * notesSpeed
    in
    div
        [ class "playNote_longSubNote"
        , style "bottom" (String.fromFloat (bottom - 20) ++ "px")
        ]
        []
