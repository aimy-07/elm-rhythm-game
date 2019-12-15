port module Page.Play exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Constants exposing (allKeyStr)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (Key(..))
import MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.CsvFileName as CsvFileName exposing (CsvFileName)
import MusicInfo.Level as Level
import MusicInfo.Mode as Mode
import Page
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime, update)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note as Note exposing (Note, NoteDto)
import Page.Play.Score as Score exposing (Score)
import Page.Play.Speed exposing (Speed)
import Process
import Rank
import Record exposing (Record, RecordDto)
import Route
import Session exposing (Session)
import Set
import Task
import Time
import User exposing (Uid)



-- MODEL


type alias Model =
    { session : Session
    , playStatus : PlayStatus
    , currentMusicInfo : Maybe MusicInfo
    , allNotes : List Note
    , lanes : List Lane
    , currentMusicTime : CurrentMusicTime
    , speed : Speed
    , score : Score
    , combo : Combo
    }


type PlayStatus
    = NotStart
    | Playing
    | Pause
    | PreFinish (Maybe Record)
    | Finish Record
    | StartCountdown
    | PauseCountdown


init : Session -> CsvFileName -> ( Model, Cmd Msg )
init session csvFileName =
    let
        audioFileName =
            CsvFileName.toAudioFileName csvFileName
    in
    ( { session = session
      , playStatus = NotStart
      , currentMusicInfo = Nothing
      , allNotes = []
      , lanes = List.map Lane.new allKeyStr
      , currentMusicTime = 0
      , speed = 0.4
      , score = Score.init
      , combo = Combo.init
      }
    , Cmd.batch
        [ getCurrentMusicInfo csvFileName
        , setMusic audioFileName
        ]
    )



-- UPDATE


type Msg
    = GotCurrentMusicInfo { musicInfoDto : MusicInfoDto, noteDtos : List NoteDto }
    | FinishedCountdown ()
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey
    | Tick Time.Posix
    | GotCurrentMusicTime CurrentMusicTime
    | GotOwnBestRecord RecordDto
    | SavedRecord ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentMusicInfo { musicInfoDto, noteDtos } ->
            let
                allNotes =
                    noteDtos
                        -- ここで無効なノーツをはじいておく
                        |> List.filter (\noteDto -> noteDto.longTime >= 0)
                        |> List.map Note.new
            in
            ( { model
                | currentMusicInfo = Just (MusicInfo.new musicInfoDto)
                , allNotes = allNotes
              }
            , Cmd.none
            )

        FinishedCountdown () ->
            case model.playStatus of
                StartCountdown ->
                    ( { model | playStatus = Playing }, startMusic () )

                PauseCountdown ->
                    let
                        nextAllNotes =
                            model.allNotes
                                |> List.map Note.updateOnKeyUp
                                |> List.filter Note.isNotDisabled

                        nextLanes =
                            List.map Lane.allUnPress model.lanes
                    in
                    ( { model
                        | playStatus = Playing
                        , allNotes = nextAllNotes
                        , lanes = nextLanes
                      }
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
                            model.currentMusicInfo
                                |> Maybe.map
                                    (\_ ->
                                        ( { model | playStatus = StartCountdown }
                                        , Process.sleep 1500
                                            |> Task.perform (\_ -> FinishedCountdown ())
                                        )
                                    )
                                |> Maybe.withDefault ( model, Cmd.none )

                        Playing ->
                            ( { model | playStatus = Pause }, pauseMusic () )

                        Pause ->
                            ( { model | playStatus = PauseCountdown }
                            , Process.sleep 1500
                                |> Task.perform (\_ -> FinishedCountdown ())
                            )

                        _ ->
                            ( model, Cmd.none )

                Just (Keyboard.Character keyStr) ->
                    let
                        nextLanes =
                            List.map (Lane.press keyStr) model.lanes
                    in
                    if model.playStatus /= Playing then
                        -- Playingの時しか判定しない
                        -- keyを押しているかどうかだけ更新する
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if List.any (Lane.isPressing keyStr) model.lanes then
                        -- すでにそのレーンのキーが押されている状態ではKeyDown判定しない
                        ( model, Cmd.none )

                    else
                        let
                            maybeHeadNote =
                                model.allNotes
                                    |> List.filter (Note.isSameKey keyStr)
                                    |> List.sortBy Note.toJustTime
                                    |> List.head
                        in
                        case maybeHeadNote of
                            Just headNote ->
                                let
                                    judge =
                                        Judge.judgeKeyDown model.currentMusicTime (Note.toJustTime headNote)

                                    nextAllNotes =
                                        model.allNotes
                                            |> List.map
                                                (\note ->
                                                    if Note.isSameNote note headNote then
                                                        Note.updateOnKeyDown judge note

                                                    else
                                                        note
                                                )
                                            |> List.filter Note.isNotDisabled

                                    nextScore =
                                        Score.updateKeyDown judge model.score

                                    nextCombo =
                                        Combo.updateKeyDown judge model.combo

                                    comboEffectCmd =
                                        Combo.comboEffectCmd model.combo nextCombo
                                in
                                ( { model
                                    | allNotes = nextAllNotes
                                    , lanes = nextLanes
                                    , score = nextScore
                                    , combo = nextCombo
                                  }
                                , Cmd.batch
                                    [ Judge.keyDownEffectCmd keyStr judge (Note.isLongNote headNote)
                                    , comboEffectCmd
                                    , playTapSound ()
                                    ]
                                )

                            Nothing ->
                                -- このレーンのノーツはもうない
                                ( { model | lanes = nextLanes }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp rawKey ->
            let
                maybeKey =
                    Keyboard.anyKeyUpper rawKey
            in
            case maybeKey of
                Just (Keyboard.Character keyStr) ->
                    let
                        nextLanes =
                            List.map (Lane.unPress keyStr) model.lanes
                    in
                    if model.playStatus /= Playing then
                        -- Playingの時しか判定しない
                        -- keyを押しているかどうかだけ更新する
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if not <| List.any (Lane.isPressing keyStr) model.lanes then
                        -- もうそのレーンのキーが押されていない状態ではKeyUp判定しない
                        ( model, Cmd.none )

                    else
                        let
                            maybeHeadNote =
                                model.allNotes
                                    |> List.filter (Note.isSameKey keyStr)
                                    |> List.sortBy Note.toJustTime
                                    |> List.head
                        in
                        case maybeHeadNote of
                            Just headNote ->
                                let
                                    nextAllNotes =
                                        model.allNotes
                                            |> List.map
                                                (\note ->
                                                    if Note.isSameNote note headNote then
                                                        Note.updateOnKeyUp note

                                                    else
                                                        note
                                                )
                                            |> List.filter Note.isNotDisabled
                                in
                                ( { model
                                    | allNotes = nextAllNotes
                                    , lanes = nextLanes
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                -- このレーンのノーツはもうない
                                ( { model | lanes = nextLanes }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick _ ->
            ( model, getCurrentMusicTime () )

        GotCurrentMusicTime updatedTime ->
            let
                nextAllNotes =
                    model.allNotes
                        |> List.map (Note.update updatedTime)
                        |> List.filter Note.isNotDisabled

                missDisabledNoteKeys =
                    model.allNotes
                        |> List.map (Note.update updatedTime)
                        |> List.filter Note.isMissDisabled
                        |> List.map Note.toKeyStr

                judgedLongNoteKeys =
                    model.allNotes
                        |> List.concatMap (Note.judgedLongNoteKeys updatedTime)

                nextScore =
                    Score.update (List.length judgedLongNoteKeys) model.score

                nextCombo =
                    let
                        hasDisabledNotes =
                            not <| List.isEmpty missDisabledNoteKeys
                    in
                    Combo.update hasDisabledNotes (List.length judgedLongNoteKeys) model.combo

                missEffectCmds =
                    missDisabledNoteKeys
                        -- 重複を削除するために一度Setに変換する
                        |> Set.fromList
                        |> Set.toList
                        |> List.map Judge.missEffectCmd
                        |> Cmd.batch

                longEffectCmds =
                    judgedLongNoteKeys
                        -- 重複を削除するために一度Setに変換する
                        |> Set.fromList
                        |> Set.toList
                        |> List.map Judge.longEffectCmd
                        |> Cmd.batch

                comboEffectCmd =
                    Combo.comboEffectCmd model.combo nextCombo

                fullTime =
                    model.currentMusicInfo
                        |> Maybe.map .fullTime
                        |> Maybe.withDefault 0

                nextPlayStatus =
                    if fullTime * 1000 <= model.currentMusicTime then
                        PreFinish Nothing

                    else
                        model.playStatus

                nextLanes =
                    if nextPlayStatus == PreFinish Nothing then
                        List.map Lane.allUnPress model.lanes

                    else
                        model.lanes

                getOwnBestRecordCmd =
                    case ( model.currentMusicInfo, Session.toUser model.session ) of
                        ( Just musicInfo, Just user ) ->
                            getOwnBestRecord { csvFileName = musicInfo.csvFileName, uid = user.uid }

                        _ ->
                            Cmd.none
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
                [ missEffectCmds
                , longEffectCmds
                , comboEffectCmd
                , getOwnBestRecordCmd
                    |> Page.cmdIf (nextPlayStatus == PreFinish Nothing)
                ]
            )

        GotOwnBestRecord recordDto ->
            let
                saveRecordCmd =
                    case ( model.currentMusicInfo, Session.toUser model.session ) of
                        ( Just musicInfo, Just user ) ->
                            saveRecord
                                { csvFileName = musicInfo.csvFileName
                                , record =
                                    { uid = user.uid
                                    , combo = Combo.unwrap model.combo
                                    , score = Score.unwrap model.score
                                    }
                                }

                        _ ->
                            Cmd.none
            in
            ( { model | playStatus = PreFinish <| Just (Record.new recordDto) }, saveRecordCmd )

        SavedRecord () ->
            case model.playStatus of
                PreFinish (Just record) ->
                    ( { model | playStatus = Finish record }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- PORT


port getCurrentMusicInfo : CsvFileName -> Cmd msg


port gotCurrentMusicInfo : ({ musicInfoDto : MusicInfoDto, noteDtos : List NoteDto } -> msg) -> Sub msg


port getCurrentMusicTime : () -> Cmd msg


port gotCurrentMusicTime : (CurrentMusicTime -> msg) -> Sub msg


port setMusic : String -> Cmd msg


port startMusic : () -> Cmd msg


port pauseMusic : () -> Cmd msg


port unPauseMusic : () -> Cmd msg


port playTapSound : () -> Cmd msg


port saveRecord : { csvFileName : CsvFileName, record : RecordDto } -> Cmd msg


port savedRecord : (() -> msg) -> Sub msg


port getOwnBestRecord : { csvFileName : CsvFileName, uid : Uid } -> Cmd msg


port gotOwnBestRecord : (RecordDto -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playStatus of
        NotStart ->
            Sub.batch
                [ gotCurrentMusicInfo GotCurrentMusicInfo
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

        PreFinish _ ->
            Sub.batch
                [ savedRecord SavedRecord
                , gotOwnBestRecord GotOwnBestRecord
                ]

        Finish _ ->
            Sub.none

        StartCountdown ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        PauseCountdown ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Play"
    , content = div [ class "mainWide" ] [ viewContents model ]
    }


viewContents : Model -> Html Msg
viewContents model =
    case model.currentMusicInfo of
        Just musicInfo ->
            div [ class "mainWide" ]
                [ div
                    [ class "play_contentsContainer" ]
                    [ div
                        [ class "play_contents" ]
                        [ viewLanes model
                        , viewNotes model
                        , viewMusicInfo musicInfo
                        , viewDisplayCircle musicInfo model
                        ]
                    , viewOverView musicInfo model
                    , div [] [ Page.viewLoaded ]
                    ]
                ]

        Nothing ->
            div [ class "play_contentsContainer" ] [ Page.viewLoading ]


viewOverView : MusicInfo -> Model -> Html msg
viewOverView musicInfo model =
    case model.playStatus of
        NotStart ->
            viewNotStart

        Playing ->
            text ""

        Pause ->
            viewPause

        PreFinish _ ->
            Page.viewLoading

        Finish ownBestRecord ->
            viewResult musicInfo ownBestRecord model

        StartCountdown ->
            viewCountdown

        PauseCountdown ->
            viewCountdown


viewLanes : Model -> Html msg
viewLanes model =
    div []
        [ div [ class "playCenterLine_judgeAreaLine outer center" ] []
        , div [ class "playCenterLine_judgeAreaLine outer left" ] []
        , div [ class "playCenterLine_judgeAreaLine outer right" ] []
        , div [] (List.map Lane.view model.lanes)
        ]


viewNotes : Model -> Html msg
viewNotes model =
    div [ class "playCenterLine_judgeLine", id "judge_area" ]
        (List.map (Note.view model.currentMusicTime model.speed) model.allNotes)


viewMusicInfo : MusicInfo -> Html msg
viewMusicInfo musicInfo =
    div [ class "playTextArea_container" ]
        [ div [ class "playTextArea_bigText" ] [ text musicInfo.musicName ]
        , div [ class "playTextArea_smallText" ] [ text musicInfo.composer ]
        , div
            [ class "playTextArea_smallText" ]
            [ span [] [ text <| Mode.toString musicInfo.mode ]
            , span [] [ text "\u{3000}" ]
            , span [] [ text <| Level.toString musicInfo.level ]
            ]
        ]


viewDisplayCircle : MusicInfo -> Model -> Html msg
viewDisplayCircle musicInfo model =
    let
        rate =
            model.currentMusicTime / (musicInfo.fullTime * 1000)

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
        , div
            [ class "playDisplay_centerTextArea" ]
            [ div [ class "playDisplay_scoreLabelText" ] [ text "- SCORE -" ]
            , div [ class "playDisplay_scoreText" ] [ text <| String.fromInt (Score.unwrap model.score) ]
            , div [ class "playDisplay_comboLabelText" ] [ text "- COMBO -" ]
            , div [ class "playDisplay_comboText", id "comboText" ] [ text <| String.fromInt (Combo.unwrap model.combo) ]
            ]
        ]


viewNotStart : Html msg
viewNotStart =
    div [ class "play_overview" ]
        [ div [ class "playOverview_startText" ] [ text "READY" ]
        , div [ class "playOverview_startSubText" ] [ text "- Press Space to Start -" ]
        ]


viewPause : Html msg
viewPause =
    div [ class "play_overview" ]
        [ div [ class "playOverview_pauseText" ] [ text "PAUSE" ]
        , div [ class "playOverview_pauseSubText" ] [ text "- Press Space to UnPause -" ]
        ]


viewCountdown : Html msg
viewCountdown =
    div [ class "play_overview" ]
        [ div [ class "playOverview_cowntdownText is-first" ] []
        , div [ class "playOverview_cowntdownText is-second" ] []
        , div [ class "playOverview_cowntdownText is-third" ] []
        ]


viewResult : MusicInfo -> Record -> Model -> Html msg
viewResult musicInfo ownBestRecord model =
    let
        isFullCombo =
            Combo.unwrap model.combo == musicInfo.maxCombo

        isHighScore =
            Score.unwrap model.score > ownBestRecord.score

        comboRank =
            Rank.newComboRank (Combo.toMaxCombo model.combo) musicInfo.maxCombo

        scoreRank =
            Rank.newScoreRank (Score.unwrap model.score) musicInfo.maxScore
    in
    div [ class "play_overview" ]
        [ div
            [ class "playOverview_contentsContainer" ]
            [ div [ class "playOverview_back" ] []
            , div [ class "playOverview_backInner" ] []
            , div [ class "playOverview_titleText" ] [ text "RESULT" ]
            , div [ class "playOverview_bigText" ] [ text musicInfo.musicName ]
            , div [ class "playOverview_smallText" ] [ text musicInfo.composer ]
            , div
                [ class "playOverview_smallText" ]
                [ span [] [ text <| Mode.toString musicInfo.mode ]
                , span [] [ text "\u{3000}" ]
                , span [] [ text <| Level.toString musicInfo.level ]
                ]
            , div
                [ class "playOverviewResultItem_container" ]
                [ div [ class "playOverviewResultItem_box" ] []
                , div [ class "playOverviewResultItem_labelText" ] [ text "COMBO" ]
                , div [ class "playOverviewResultItem_rankText" ] [ text <| Rank.toString comboRank ]
                , div [ class "playOverviewResultItem_effectText" ] [ text "Full Combo!!" ]
                    |> Page.viewIf isFullCombo
                , div
                    [ class "playOverviewResultItem_textContainer" ]
                    [ span
                        [ class "playOverviewResultItem_resultText" ]
                        [ text <| String.fromInt (Combo.toMaxCombo model.combo) ]
                    , span
                        [ class "playOverviewResultItem_maxText" ]
                        [ text <| " / " ++ String.fromInt musicInfo.maxCombo ]
                    ]
                , div [ class "playOverviewResultItem_line" ] []
                ]
            , div
                [ class "playOverviewResultItem_container" ]
                [ div [ class "playOverviewResultItem_box" ] []
                , div [ class "playOverviewResultItem_labelText" ] [ text "SCORE" ]
                , div [ class "playOverviewResultItem_rankText" ] [ text <| Rank.toString scoreRank ]
                , div [ class "playOverviewResultItem_effectText" ] [ text "High Score!!" ]
                    |> Page.viewIf isHighScore
                , div
                    [ class "playOverviewResultItem_textContainer" ]
                    [ span
                        [ class "playOverviewResultItem_resultText" ]
                        [ text <| String.fromInt (Score.unwrap model.score) ]
                    , span
                        [ class "playOverviewResultItem_maxText" ]
                        [ text <| " / " ++ String.fromInt musicInfo.maxScore ]
                    ]
                , div [ class "playOverviewResultItem_line" ] []
                ]
            , a
                [ class "playOverviewResultItem_backBtn", Route.href Route.Home ]
                [ text "- Back to Home -" ]
            ]
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
