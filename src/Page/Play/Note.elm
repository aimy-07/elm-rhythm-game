module Page.Play.Note exposing
    ( Note
    , NoteDto
    , isDisabled
    , isLongJudging
    , isLongNote
    , isMissDisabled
    , isNotDisabled
    , isSameKey
    , isSameNote
    , judgedLongNoteKeys
    , new
    , toJustTime
    , toKeyStr
    , toLongTime
    , update
    , updateOnKeyDown
    , updateOnKeyUp
    , view
    )

import Constants exposing (longTimeDuration, longTimeOffset)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.KeyStr exposing (KeyStr)
import Page.Play.Lane as Lane
import Page.Play.Note.JustTime exposing (JustTime)
import Setting.NotesSpeed exposing (NotesSpeed)
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

        _ ->
            0


toLongTime : Note -> LongTime
toLongTime note =
    case note of
        LongNote { longTime } ->
            longTime

        _ ->
            0


toLongSubJustTimeList : Note -> List LongSubJustTime
toLongSubJustTimeList note =
    case note of
        LongNote { subJustTimeList } ->
            subJustTimeList

        _ ->
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

        _ ->
            False


isLongJudging : Note -> Bool
isLongJudging note =
    case note of
        LongNote { isJudging } ->
            isJudging

        _ ->
            False


isDisabled : Note -> Bool
isDisabled note =
    case note of
        Disabled _ ->
            True

        _ ->
            False


isNotDisabled : Note -> Bool
isNotDisabled note =
    not <| isDisabled note


isMissDisabled : Note -> Bool
isMissDisabled note =
    case note of
        Disabled { isMiss } ->
            isMiss

        _ ->
            False


{-| 毎フレームUpdateごとに、ノーツの状態を更新する
-}
update : CurrentMusicTime -> Note -> Note
update currentMusicTime note =
    case note of
        SingleNote _ ->
            if Judge.isOverMiss currentMusicTime (toJustTime note) then
                Disabled { keyStr = toKeyStr note, isMiss = True }

            else
                note

        LongNote note_ ->
            if isLongJudging note then
                let
                    nextSubJustTimeList =
                        toLongSubJustTimeList note
                            |> List.filter
                                (\subJustTime ->
                                    not <| Judge.isOverJustTime currentMusicTime subJustTime
                                )

                    endTime =
                        toJustTime note + toLongTime note
                in
                if List.isEmpty nextSubJustTimeList && endTime < currentMusicTime then
                    Disabled { keyStr = toKeyStr note, isMiss = False }

                else
                    LongNote { note_ | subJustTimeList = nextSubJustTimeList }

            else if Judge.isOverMiss currentMusicTime (toJustTime note) then
                Disabled { keyStr = toKeyStr note, isMiss = True }

            else
                note

        Disabled _ ->
            note


{-| 毎フレームUpdateごとに、伸ばしているノングノーツのスコア・コンボ増分を計算するための関数
-}
judgedLongNoteKeys : CurrentMusicTime -> Note -> List KeyStr
judgedLongNoteKeys currentMusicTime note =
    case note of
        LongNote _ ->
            if isLongJudging note then
                toLongSubJustTimeList note
                    |> List.filter (Judge.isOverJustTime currentMusicTime)
                    |> List.map (\_ -> toKeyStr note)

            else
                []

        _ ->
            []


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

        _ ->
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
