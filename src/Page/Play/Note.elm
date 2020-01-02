module Page.Play.Note exposing
    ( Note
    , NoteDto
    , computeMaxCombo
    , computeMaxScore
    , isDisabled
    , isLongJudging
    , isLongNote
    , isMissDisabled
    , isNotDisabled
    , isSameKey
    , isSameNote
    , judgedLongNoteKeys
    , maybeHeadNote
    , new
    , toJustTime
    , toKeyStr
    , toLongTime
    , update
    , updateHeadNote
    , updateOnKeyDown
    , updateOnKeyUp
    , view
    )

import Constants exposing (longScore, longTimeDuration, longTimeOffset, perfectScore)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.KeyStr exposing (KeyStr)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note.JustTime exposing (JustTime)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (viewIf)


type Note
    = SingleNote
        { keyStr : KeyStr
        , justTime : JustTime
        }
    | LongNote
        { keyStr : KeyStr
        , justTime : JustTime
        , longTime : LongTime
        , subJustTimeList : List LongSubJustTime
        , isJudging : Bool
        }
    | Disabled
        { keyStr : KeyStr
        , isMiss : Bool
        }


type alias LongTime =
    Float


type alias LongSubJustTime =
    JustTime


type alias NoteDto =
    { keyStr : String
    , justTime : Float
    , longTime : Float
    }


new : NoteDto -> Note
new noteDto =
    if noteDto.longTime < longTimeOffset then
        SingleNote
            { keyStr = noteDto.keyStr
            , justTime = noteDto.justTime
            }

    else
        let
            subJustTimeList =
                Basics.floor ((noteDto.longTime - longTimeOffset) / longTimeDuration)
                    |> List.range 0
                    |> List.map
                        (\index ->
                            if index == 0 then
                                noteDto.justTime + longTimeOffset

                            else
                                noteDto.justTime + longTimeOffset + longTimeDuration * Basics.toFloat index
                        )
        in
        LongNote
            { keyStr = noteDto.keyStr
            , justTime = noteDto.justTime
            , longTime = noteDto.longTime
            , subJustTimeList = subJustTimeList
            , isJudging = False
            }


computeMaxScore : List Note -> Int
computeMaxScore notes =
    notes
        |> List.map (\note -> perfectScore + List.length (toLongSubJustTimeList note) * longScore)
        |> List.sum


computeMaxCombo : List Note -> Int
computeMaxCombo notes =
    notes
        |> List.map (\note -> 1 + List.length (toLongSubJustTimeList note))
        |> List.sum


toKeyStr : Note -> KeyStr
toKeyStr note =
    case note of
        SingleNote { keyStr } ->
            keyStr

        LongNote { keyStr } ->
            keyStr

        Disabled { keyStr } ->
            keyStr


toJustTime : Note -> JustTime
toJustTime note =
    case note of
        SingleNote { justTime } ->
            justTime

        LongNote { justTime } ->
            justTime

        Disabled _ ->
            0


toLongTime : Note -> LongTime
toLongTime note =
    case note of
        LongNote { longTime } ->
            longTime

        SingleNote _ ->
            0

        Disabled _ ->
            0


toLongSubJustTimeList : Note -> List LongSubJustTime
toLongSubJustTimeList note =
    case note of
        LongNote { subJustTimeList } ->
            subJustTimeList

        SingleNote _ ->
            []

        Disabled _ ->
            []


isSameKey : KeyStr -> Note -> Bool
isSameKey keyStr note =
    keyStr == toKeyStr note


isSameNote : Note -> Note -> Bool
isSameNote a b =
    not (isDisabled a) && not (isDisabled b) && (toKeyStr a == toKeyStr b) && (toJustTime a == toJustTime b)


isLongNote : Note -> Bool
isLongNote note =
    case note of
        LongNote _ ->
            True

        SingleNote _ ->
            False

        Disabled _ ->
            False


isLongJudging : Note -> Bool
isLongJudging note =
    case note of
        LongNote { isJudging } ->
            isJudging

        SingleNote _ ->
            False

        Disabled _ ->
            False


isDisabled : Note -> Bool
isDisabled note =
    case note of
        Disabled _ ->
            True

        SingleNote _ ->
            False

        LongNote _ ->
            False


isNotDisabled : Note -> Bool
isNotDisabled note =
    not <| isDisabled note


isMissDisabled : Note -> Bool
isMissDisabled note =
    case note of
        Disabled { isMiss } ->
            isMiss

        SingleNote _ ->
            False

        LongNote _ ->
            False


{-| 同じレーンのノーツの中で、現在先頭にあるノーツを取得する
-}
maybeHeadNote : KeyStr -> List Note -> Maybe Note
maybeHeadNote keyStr allNotes =
    allNotes
        |> List.filter (isSameKey keyStr)
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


{-| 毎フレームUpdateごとに、ノーツの状態を更新する
-}
update : CurrentMusicTime -> List Lane -> Note -> Note
update currentMusicTime lanes note =
    case note of
        SingleNote _ ->
            if Judge.isOverMiss currentMusicTime (toJustTime note) then
                -- 判定可能領域内でKeyDownをせず、Missになった
                Disabled { keyStr = toKeyStr note, isMiss = True }

            else
                note

        LongNote note_ ->
            if isLongJudging note then
                -- ロングノーツ判定中
                let
                    nextSubJustTimeList =
                        toLongSubJustTimeList note
                            |> List.filter
                                (\subJustTime ->
                                    not <| Judge.isOverJustTime currentMusicTime subJustTime
                                )

                    endTime =
                        toJustTime note + toLongTime note

                    isPressing =
                        Lane.pressingKeyStrs (toKeyStr note) lanes
                            |> List.member (toKeyStr note)
                in
                if List.isEmpty nextSubJustTimeList && endTime < currentMusicTime then
                    -- 長押しを最後までしてロングノーツの判定を終えた
                    Disabled { keyStr = toKeyStr note, isMiss = False }

                else if not isPressing then
                    -- ロングノーツ判定中にPauseし、Pause中にKeyUpした
                    Disabled { keyStr = toKeyStr note, isMiss = False }

                else
                    -- 長押しを継続している
                    LongNote { note_ | subJustTimeList = nextSubJustTimeList }

            else if Judge.isOverMiss currentMusicTime (toJustTime note) then
                -- 判定可能領域内でKeyDownをせず、Missになった
                Disabled { keyStr = toKeyStr note, isMiss = True }

            else
                note

        Disabled _ ->
            note


{-| 毎フレームUpdateごとに、伸ばしているノングノーツのスコア・コンボ増分を計算するための関数
-}
judgedLongNoteKeys : CurrentMusicTime -> List Lane -> Note -> List KeyStr
judgedLongNoteKeys currentMusicTime lanes note =
    let
        isPressing =
            Lane.pressingKeyStrs (toKeyStr note) lanes
                |> List.member (toKeyStr note)
    in
    toLongSubJustTimeList note
        |> List.filter
            (\subJustTime ->
                Judge.isOverJustTime currentMusicTime subJustTime && isPressing
            )
        |> List.map (\_ -> toKeyStr note)


{-| KeyDown時に、判定結果に応じてノーツの状態を更新する
-}
updateOnKeyDown : Judge -> Note -> Note
updateOnKeyDown judge note =
    case note of
        SingleNote _ ->
            if judge /= Invalid then
                if judge /= Miss then
                    Disabled { keyStr = toKeyStr note, isMiss = True }

                else
                    Disabled { keyStr = toKeyStr note, isMiss = False }

            else
                note

        LongNote note_ ->
            -- keydownでロングノーツがPerfect, Great, Good判定になったときにisLongJudging = Trueにする
            -- それ以外のタイミングでisLongJudging = Trueになることはない
            if judge /= Invalid then
                if judge == Miss then
                    Disabled { keyStr = toKeyStr note, isMiss = True }

                else
                    LongNote { note_ | isJudging = True }

            else
                note

        Disabled _ ->
            note


{-| KeyUp時に、isLongJudgingのノーツをDisabledに更新する
-}
updateOnKeyUp : Note -> Note
updateOnKeyUp note =
    case note of
        LongNote _ ->
            if isLongJudging note then
                Disabled { keyStr = toKeyStr note, isMiss = True }

            else
                note

        SingleNote _ ->
            note

        Disabled _ ->
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

        LongNote _ ->
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
                    , style "bottom" (String.fromFloat bottom ++ "px")
                    , style "height" (String.fromFloat height ++ "px")
                    ]
                    []
                ]

        Disabled _ ->
            text ""
