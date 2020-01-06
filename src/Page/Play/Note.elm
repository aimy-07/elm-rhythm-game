module Page.Play.Note exposing
    ( Note
    , NoteDto
    , computeMaxCombo
    , computeMaxScore
    , headNoteJudge
    , headNoteJudgeEffect
    , headNotes
    , isDisabled
    , isLongNote
    , maybeHeadNote
    , new
    , toJustTime
    , update
    , updateHeadNote
    , updateKeyDown
    , updateKeyUp
    , view
    )

import Constants exposing (allKeyStrList, longTimeDuration, longTimeOffset, perfectScore)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge as Judge exposing (Judge(..), JudgeEffect)
import Page.Play.KeyStr exposing (KeyStr)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note.JustTime exposing (JustTime)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (viewIf)


type Note
    = SingleNote
        { keyStr : KeyStr
        , justTime : JustTime
        , noteStatus : NoteStatus
        }
    | LongNote
        { keyStr : KeyStr
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
    { keyStr : String
    , justTime : Float
    , longTime : Float
    }


new : NoteDto -> Note
new { keyStr, justTime, longTime } =
    if longTime < longTimeDuration + longTimeOffset then
        SingleNote
            { keyStr = keyStr
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
            { keyStr = keyStr
            , justTime = justTime
            , longTime = longTime
            , longSubNotes = longSubNotes
            , noteStatus = NotJudged
            }


computeMaxScore : List Note -> Int
computeMaxScore notes =
    notes
        |> List.map (\note -> 1 + List.length (toLongSubNotes note))
        |> List.sum
        |> (*) perfectScore


computeMaxCombo : List Note -> Int
computeMaxCombo notes =
    notes
        |> List.map (\note -> 1 + List.length (toLongSubNotes note))
        |> List.sum


toKeyStr : Note -> KeyStr
toKeyStr note =
    case note of
        SingleNote { keyStr } ->
            keyStr

        LongNote { keyStr } ->
            keyStr


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


isSameNote : Note -> Note -> Bool
isSameNote a b =
    (not << isDisabled) a && (not << isDisabled) b && (toKeyStr a == toKeyStr b) && (toJustTime a == toJustTime b)


{-| 同じレーンのノーツの中で、現在先頭にあるノーツを取得する
-}
maybeHeadNote : KeyStr -> List Note -> Maybe Note
maybeHeadNote keyStr allNotes =
    allNotes
        |> List.filter (toKeyStr >> (==) keyStr)
        |> List.sortBy toJustTime
        |> List.head


{-| 同じレーンのノーツの中で、現在先頭にあるノーツのみを更新する
-}
updateHeadNote : KeyStr -> (Note -> Note) -> List Note -> List Note
updateHeadNote keyStr updateNote allNotes =
    maybeHeadNote keyStr allNotes
        |> Maybe.map
            (\headNote ->
                allNotes
                    |> List.map
                        (\note ->
                            if isSameNote note headNote then
                                updateNote note

                            else
                                note
                        )
            )
        |> Maybe.withDefault allNotes


{-| 全レーンにおいて、各レーンの先頭のノーツを取得する
-}
headNotes : List Note -> List Note
headNotes allNotes =
    allKeyStrList
        |> List.map (\keyStr -> maybeHeadNote keyStr allNotes)
        |> List.filterMap identity


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
    { keyStr = toKeyStr headNote
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
                    Lane.pressingKeyStrs (toKeyStr note) lanes
                        |> List.member (toKeyStr note)
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
            if judge == Perfect || judge == Great || judge == Good then
                -- KeyDownでLost or Missになることはない
                SingleNote { singleNote | noteStatus = Judged }

            else
                note

        LongNote longNote ->
            if judge == Perfect || judge == Great || judge == Good then
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
            Lane.leftFromKeyStr <| toKeyStr note

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
                    , style "bottom" (String.fromFloat (bottom - 20) ++ "px")
                    ]
                    []
                ]

        LongNote { noteStatus } ->
            let
                clsIsLongDisabled =
                    if noteStatus == LongJudging Lost then
                        "is-disabled"

                    else
                        ""
            in
            div
                [ style "left" (String.fromInt left ++ "px")
                , style "position" "absolute"
                ]
                [ div
                    [ class "playNote_note long"
                    , style "bottom" (String.fromFloat (bottom - 20) ++ "px")
                    ]
                    []
                    |> viewIf (not <| isLongJudging note)
                , div
                    [ class "playNote_longLine"
                    , class clsIsLongDisabled
                    , style "bottom" (String.fromFloat bottom ++ "px")
                    , style "height" (String.fromFloat height ++ "px")
                    ]
                    []
                , div
                    []
                    (toLongSubNotes note
                        |> List.map (viewLongSubNote currentMusicTime notesSpeed)
                    )
                ]



-- TODO: デバッグ用


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
