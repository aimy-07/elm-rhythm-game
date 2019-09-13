port module Page.Play exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.CsvFileName exposing (CsvFileName)
import MusicInfo.Mode as Mode exposing (Mode)
import Page
import Page.Play.AllNotes as AllNotes exposing (AllNotes, AllNotesDto)
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime, updateCurrentMusicTime)
import Page.Play.JudgeEffect as JudgeEffect exposing (JudgeEffect)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Lanes as Lanes exposing (Lanes)
import Page.Play.LongNoteLine as LongNoteLine exposing (EndTime, LongNoteLine)
import Page.Play.Note as Note exposing (JustTime, Note)
import Page.Play.NotesPerLane as NotesPerLane exposing (NotesPerLane)
import Page.Play.PlayingMusicInfo as PlayingMusicInfo exposing (PlayingMusicInfo)
import Page.Play.Score as Score exposing (Score)
import Page.Play.Speed exposing (Speed)
import Rank
import Route
import Session exposing (Session)
import Task
import Time



-- MODEL


type alias Model =
    { session : Session
    , playStatus : PlayStatus
    , playingMusicInfo : PlayingMusicInfo
    , allNotes : AllNotes
    , lanes : Lanes
    , currentMusicTime : CurrentMusicTime
    , speed : Speed
    , score : Score
    , combo : Combo
    }


type PlayStatus
    = NotStart
    | Playing
    | Pause
    | Finish
    | StartCountdown
    | PauseCountdown


init : Session -> CsvFileName -> ( Model, Cmd Msg )
init session csvFileName =
    ( { session = session
      , playStatus = NotStart
      , playingMusicInfo = PlayingMusicInfo.init
      , allNotes = AllNotes.init
      , lanes = Lanes.init
      , currentMusicTime = 0
      , speed = 0.4
      , score = Score.init
      , combo = Combo.init
      }
    , Cmd.batch
        [ getPlayingMusicInfo csvFileName
        , getAllNotes csvFileName
        ]
    )



-- UPDATE


type Msg
    = GotPlayingMusicInfo MusicInfoDto
    | GotAllNotes AllNotesDto
    | PlayedCountdownAnim ()
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey
    | Tick Time.Posix
    | GotCurrentMusicTime CurrentMusicTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlayingMusicInfo musicInfoDto ->
            let
                nextPlayingMusicInfo =
                    PlayingMusicInfo.new musicInfoDto
            in
            ( { model | playingMusicInfo = nextPlayingMusicInfo }
            , Cmd.none
            )

        GotAllNotes allNotesDto ->
            let
                nextAllNotes =
                    AllNotes.new allNotesDto
            in
            ( { model | allNotes = nextAllNotes }
            , Cmd.none
            )

        PlayedCountdownAnim () ->
            case model.playStatus of
                StartCountdown ->
                    ( { model | playStatus = Playing }
                    , startMusic ()
                    )

                PauseCountdown ->
                    let
                        nextAllNotes =
                            model.allNotes
                                |> AllNotes.updateNotesUnPause model.lanes
                    in
                    ( { model | playStatus = Playing, allNotes = nextAllNotes }
                    , unPauseMusic ()
                    )

                _ ->
                    ( model, Cmd.none )

        KeyDown rawKey ->
            let
                maybeKey =
                    Keyboard.anyKeyUpper rawKey
            in
            case maybeKey of
                Just Keyboard.Spacebar ->
                    case model.playStatus of
                        NotStart ->
                            if PlayingMusicInfo.isLoaded model.playingMusicInfo then
                                ( { model | playStatus = StartCountdown }, playCountdownAnim () )

                            else
                                ( model, Cmd.none )

                        Playing ->
                            ( { model | playStatus = Pause }, pauseMusic () )

                        Pause ->
                            ( { model | playStatus = PauseCountdown }, playCountdownAnim () )

                        _ ->
                            ( model, Cmd.none )

                Just (Keyboard.Character keyStr) ->
                    let
                        notesPerLane =
                            model.allNotes
                                |> AllNotes.toNotesPerLane keyStr
                    in
                    if model.playStatus /= Playing then
                        -- Playingの時しか判定しない
                        -- keyを押しているかどうかだけ更新する
                        let
                            nextLanes =
                                Lanes.updateKeyDown keyStr model.lanes
                        in
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if Lanes.isPressing keyStr model.lanes then
                        -- すでにそのレーンのキーが押されている状態ではKeyDown判定しない
                        ( model, Cmd.none )

                    else
                        let
                            judgeKind =
                                NotesPerLane.maybeHeadNote notesPerLane
                                    |> JudgeKind.judgeKeyDown model.currentMusicTime

                            nextAllNotes =
                                model.allNotes
                                    |> Page.updateIf
                                        (not <| JudgeKind.isInvalid judgeKind)
                                        (AllNotes.updateNotesKeyDown keyStr)

                            judgeEffectCmd =
                                JudgeEffect.keyDownEffectCmd judgeKind notesPerLane
                                    |> Page.cmdIf (not <| JudgeKind.isInvalid judgeKind)

                            comboEffectCmd =
                                playComboEffectAnim ()
                                    |> Page.cmdIf (not <| JudgeKind.isInvalid judgeKind)

                            playTapSoundCmd =
                                playTapSound ()
                                    |> Page.cmdIf (not <| JudgeKind.isInvalid judgeKind)

                            nextLanes =
                                Lanes.updateKeyDown keyStr model.lanes
                        in
                        ( { model
                            | allNotes = nextAllNotes
                            , lanes = nextLanes
                            , score = Score.add judgeKind model.score
                            , combo = Combo.update judgeKind model.combo
                          }
                        , Cmd.batch
                            [ judgeEffectCmd
                            , comboEffectCmd
                            , playTapSoundCmd
                            ]
                        )

                _ ->
                    ( model, Cmd.none )

        KeyUp rawKey ->
            let
                maybeKey =
                    Keyboard.anyKeyUpper rawKey
            in
            case maybeKey of
                Just (Keyboard.Character keyStr) ->
                    if model.playStatus /= Playing then
                        -- Playingの時しか判定しない
                        -- keyを押しているかどうかだけ更新する
                        let
                            nextLanes =
                                Lanes.updateKeyUp keyStr model.lanes
                        in
                        ( { model | lanes = nextLanes }
                        , Cmd.none
                        )

                    else
                        let
                            nextAllNotes =
                                model.allNotes
                                    |> AllNotes.updateNotesKeyUp keyStr

                            nextLanes =
                                Lanes.updateKeyUp keyStr model.lanes
                        in
                        ( { model | allNotes = nextAllNotes, lanes = nextLanes }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        Tick _ ->
            ( model, getCurrentMusicTime () )

        GotCurrentMusicTime updatedTime ->
            let
                nextAllNotes =
                    -- 先にMissの処理を行う
                    model.allNotes
                        |> AllNotes.updateNotesOverMiss updatedTime
                        |> AllNotes.updateLongNoteLines updatedTime

                nextScore =
                    nextAllNotes
                        |> AllNotes.map
                            (\notesPerLane ->
                                Score.calcLongScore notesPerLane
                            )
                        |> List.foldl (+) 0
                        |> Score.addLong model.score

                nextCombo =
                    let
                        -- Missがあったら先にCombo = 0を実行する
                        hasOverMissNotes =
                            AllNotes.hasOverMissNotes updatedTime model.allNotes

                        updatedComboForMiss =
                            model.combo
                                |> Page.updateIf hasOverMissNotes (Combo.update JudgeKind.miss)
                    in
                    nextAllNotes
                        |> AllNotes.map
                            (\notesPerLane ->
                                Combo.calcLongCombo notesPerLane
                            )
                        |> List.foldl (+) 0
                        |> Combo.addLong updatedComboForMiss

                missEffectCmd =
                    model.allNotes
                        |> AllNotes.map
                            (\notesPerLane ->
                                JudgeEffect.missEffectCmd updatedTime notesPerLane
                            )
                        |> Cmd.batch

                longEffectCmd =
                    nextAllNotes
                        |> AllNotes.map
                            (\notesPerLane ->
                                JudgeEffect.longEffectCmd notesPerLane
                            )
                        |> Cmd.batch

                comboEffectCmd =
                    let
                        addedCombo =
                            nextAllNotes
                                |> AllNotes.map
                                    (\notesPerLane ->
                                        Combo.calcLongCombo notesPerLane
                                    )
                                |> List.foldl (+) 0
                    in
                    playComboEffectAnim ()
                        |> Page.cmdIf (addedCombo /= 0)

                fullTime =
                    MusicInfo.toFullTime <| PlayingMusicInfo.toMusicInfo model.playingMusicInfo

                nextPlayStatus =
                    model.playStatus
                        |> Page.updateIf
                            (fullTime <= model.currentMusicTime)
                            (always Finish)

                nextLanes =
                    model.lanes
                        |> Page.updateIf
                            (nextPlayStatus == Finish)
                            Lanes.updateFinish
            in
            ( { model
                | currentMusicTime = updatedTime
                , playStatus = nextPlayStatus
                , lanes = nextLanes
                , allNotes = nextAllNotes
                , score = nextScore
                , combo = nextCombo
              }
            , Cmd.batch
                [ missEffectCmd
                , longEffectCmd
                , comboEffectCmd
                ]
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Play"
    , content =
        if PlayingMusicInfo.isLoaded model.playingMusicInfo then
            div [ class "mainWide" ]
                [ div [ class "play_contentsContainer" ]
                    [ div [ class "play_contents" ]
                        [ Lanes.view model.lanes
                        , AllNotes.view
                            model.currentMusicTime
                            model.speed
                            model.allNotes
                        , viewDisplayCircle model
                        , viewMusicInfo model
                        ]
                    ]
                , viewNotStart model
                    |> Page.viewIf (model.playStatus == NotStart || model.playStatus == StartCountdown)
                , viewPause model
                    |> Page.viewIf (model.playStatus == Pause || model.playStatus == PauseCountdown)
                , viewResult model
                    |> Page.viewIf (model.playStatus == Finish)
                ]

        else
            text ""
    }


viewMusicInfo : Model -> Html msg
viewMusicInfo model =
    let
        musicInfo =
            PlayingMusicInfo.toMusicInfo model.playingMusicInfo
    in
    div [ class "playTextArea_container" ]
        [ div
            [ class "playTextArea_bigText" ]
            [ text <| MusicInfo.toMusicName musicInfo ]
        , div
            [ class "playTextArea_smallText" ]
            [ text <| MusicInfo.toComposer musicInfo ]
        , div
            [ class "playTextArea_smallText" ]
            [ span [] [ text <| Mode.unwrap <| MusicInfo.toMode musicInfo ]
            , span [] [ text "\u{3000}" ]
            , span [] [ text <| MusicInfo.toStringLevel musicInfo ]
            ]
        ]


viewDisplayCircle : Model -> Html msg
viewDisplayCircle model =
    let
        fullTime =
            MusicInfo.toFullTime <| PlayingMusicInfo.toMusicInfo model.playingMusicInfo

        rate =
            model.currentMusicTime / fullTime

        half1Rotate =
            if rate <= 0.5 then
                "rotate(0deg)"

            else if rate >= 1 then
                "rotate(180deg)"

            else
                "rotate(" ++ String.fromFloat (360 * (rate - 0.5)) ++ "deg)"

        half2Rotate =
            if rate <= 0.5 then
                "rotate(" ++ String.fromFloat (360 * rate) ++ "deg)"

            else if rate <= 0 then
                "rotate(0deg)"

            else
                "rotate(360deg)"

        half2Color =
            if rate <= 0.5 then
                "#999"

            else
                "#EEA41D"
    in
    div [ class "playDisplay_circle" ]
        [ div [ class "playDisplay_circle-inner" ] []
        , div [ class "half1", style "transform" half1Rotate ] []
        , div [ class "half2", style "transform" half2Rotate, style "background-color" half2Color ] []
        , div [ class "playDisplay_centerTextArea" ]
            [ div [ class "playDisplay_scoreLabelText" ] [ text "- SCORE -" ]
            , div
                [ class "playDisplay_scoreText" ]
                [ text <| String.fromInt <| Score.unwrap model.score ]
            , div [ class "playDisplay_comboLabelText" ] [ text "- COMBO -" ]
            , div
                [ class "playDisplay_comboText", id "comboText" ]
                [ text <| String.fromInt <| Combo.unwrap model.combo ]
            ]
        ]


viewNotStart : Model -> Html msg
viewNotStart model =
    div [ class "play_overview" ]
        [ div [ class "playOverview_startText" ] [ text "READY" ]
            |> Page.viewIf (model.playStatus == NotStart)
        , div [ class "playOverview_startSubText" ] [ text "- Press Space to Start -" ]
            |> Page.viewIf (model.playStatus == NotStart)
        , div [ class "playOverview_cowntdownText", id "playOverview_cowntdownText" ] []
        ]


viewPause : Model -> Html msg
viewPause model =
    div [ class "play_overview" ]
        [ div [ class "playOverview_pauseText" ] [ text "PAUSE" ]
            |> Page.viewIf (model.playStatus == Pause)
        , div [ class "playOverview_pauseSubText" ] [ text "- Press Space to UnPause -" ]
            |> Page.viewIf (model.playStatus == Pause)
        , div [ class "playOverview_cowntdownText", id "playOverview_cowntdownText" ] []
        ]


viewResult : Model -> Html msg
viewResult model =
    let
        musicInfo =
            PlayingMusicInfo.toMusicInfo model.playingMusicInfo

        maxCombo =
            MusicInfo.toMaxCombo musicInfo

        isFullCombo =
            Combo.unwrap model.combo == MusicInfo.toMaxCombo musicInfo

        maxScore =
            MusicInfo.toMaxScore musicInfo

        -- TODO: ハイスコアかどうかの判定をする
        isHighScore =
            True
    in
    div [ class "play_overview" ]
        [ div [ class "playOverview_contentsContainer" ]
            [ div [ class "playOverview_back" ] []
            , div [ class "playOverview_backInner" ] []
            , div [ class "playOverview_titleText" ] [ text "RESULT" ]
            , div [ class "playOverview_bigText" ]
                [ text <| MusicInfo.toMusicName musicInfo ]
            , div [ class "playOverview_smallText" ]
                [ text <| MusicInfo.toComposer musicInfo ]
            , div [ class "playOverview_smallText" ]
                [ span [] [ text <| Mode.unwrap <| MusicInfo.toMode musicInfo ]
                , span [] [ text "\u{3000}" ]
                , span [] [ text <| MusicInfo.toStringLevel musicInfo ]
                ]
            , div [ class "playOverviewResultItem_container" ]
                [ div [ class "playOverviewResultItem_box" ] []
                , div [ class "playOverviewResultItem_labelText" ] [ text "COMBO" ]
                , div [ class "playOverviewResultItem_rankText" ]
                    [ text <|
                        Rank.toString <|
                            Rank.newComboRank (Combo.unwrap model.combo) maxCombo
                    ]
                , div [ class "playOverviewResultItem_effectText" ] [ text "Full Combo!" ]
                    |> Page.viewIf isFullCombo
                , div [ class "playOverviewResultItem_textContainer" ]
                    [ span [ class "playOverviewResultItem_resultText" ]
                        [ text <| String.fromInt <| Combo.unwrap model.combo ]
                    , span [ class "playOverviewResultItem_maxText" ]
                        [ text <| " / " ++ String.fromInt maxCombo ]
                    ]
                , div [ class "playOverviewResultItem_line" ] []
                ]
            , div [ class "playOverviewResultItem_container" ]
                [ div [ class "playOverviewResultItem_box" ] []
                , div [ class "playOverviewResultItem_labelText" ] [ text "SCORE" ]
                , div [ class "playOverviewResultItem_rankText" ]
                    [ text <|
                        Rank.toString <|
                            Rank.newScoreRank (Score.unwrap model.score) maxScore
                    ]
                , div [ class "playOverviewResultItem_effectText" ] [ text "High Score!" ]
                    |> Page.viewIf isHighScore
                , div [ class "playOverviewResultItem_textContainer" ]
                    [ span [ class "playOverviewResultItem_resultText" ]
                        [ text <| String.fromInt <| Score.unwrap model.score ]
                    , span [ class "playOverviewResultItem_maxText" ]
                        [ text <| " / " ++ String.fromInt maxScore ]
                    ]
                , div [ class "playOverviewResultItem_line" ] []
                ]
            , a
                [ class "playOverviewResultItem_backBtn"
                , Route.href Route.Home
                ]
                [ text "- Back to Home -" ]
            ]
        ]



-- PORT


port getPlayingMusicInfo : CsvFileName -> Cmd msg


port gotPlayingMusicInfo : (MusicInfoDto -> msg) -> Sub msg


port getAllNotes : CsvFileName -> Cmd msg


port gotAllNotes : (AllNotesDto -> msg) -> Sub msg


port getCurrentMusicTime : () -> Cmd msg


port gotCurrentMusicTime : (CurrentMusicTime -> msg) -> Sub msg


port startMusic : () -> Cmd msg


port pauseMusic : () -> Cmd msg


port unPauseMusic : () -> Cmd msg


port playTapSound : () -> Cmd msg


port playComboEffectAnim : () -> Cmd msg


port playCountdownAnim : () -> Cmd msg


port playedCountdownAnim : (() -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playStatus of
        NotStart ->
            Sub.batch
                [ gotAllNotes GotAllNotes
                , gotPlayingMusicInfo GotPlayingMusicInfo
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Playing ->
            Sub.batch
                [ Time.every 10 Tick
                , gotCurrentMusicTime GotCurrentMusicTime
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Pause ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Finish ->
            Sub.none

        StartCountdown ->
            Sub.batch
                [ playedCountdownAnim PlayedCountdownAnim
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        PauseCountdown ->
            Sub.batch
                [ playedCountdownAnim PlayedCountdownAnim
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
