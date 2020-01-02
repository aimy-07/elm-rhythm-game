module Page.Play exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toAllMusicData
    , toAudioLoadingS
    , toSession
    , update
    , view
    )

import AllMusicData exposing (AllMusicData)
import AllMusicData.MusicData as MusicData exposing (MusicData)
import AllMusicData.MusicData.CsvFileName exposing (CsvFileName)
import AllMusicData.MusicData.Level as Level
import AllMusicData.MusicData.Mode as Mode
import AudioManager
import AudioManager.AudioLoadingS exposing (AudioLoadingS)
import AudioManager.BGM as BGM
import Constants exposing (allKeyStrList, notesSpeedDefault, tweetText)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, id, style, target)
import Keyboard exposing (Key(..))
import Page
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note as Note exposing (Note)
import Page.Play.Score as Score exposing (Score)
import Process
import Rank
import Record
import Route
import Session exposing (Session)
import Set
import Task
import Time
import UserSetting exposing (UserSetting)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (cmdIf, viewIf)



-- MODEL


type alias Model =
    { session : Session
    , allMusicData : AllMusicData
    , audioLoadingS : AudioLoadingS
    , currentMusicData : MusicData
    , userSetting : UserSetting
    , playStatus : PlayStatus
    , allNotes : List Note
    , currentMusicTime : CurrentMusicTime
    , score : Score
    , combo : Combo
    , lanes : List Lane
    }



-- TODO: 切り出せない？


type PlayStatus
    = Ready
    | Playing
    | Pause
    | PreFinish
    | Finish Bool
    | StartCountdown
    | PauseCountdown


init : Session -> AllMusicData -> AudioLoadingS -> Maybe CsvFileName -> Maybe UserSetting -> ( Model, Cmd Msg )
init session allMusicData audioLoadingS maybeCsvFileName maybeUserSetting =
    let
        maybeCurrentMusicData =
            case maybeCsvFileName of
                Just csvFileName ->
                    AllMusicData.findByCsvFileName csvFileName allMusicData

                Nothing ->
                    Nothing
    in
    case ( maybeCurrentMusicData, maybeUserSetting ) of
        ( Just currentMusicData, Just userSetting ) ->
            ( initModel session allMusicData audioLoadingS currentMusicData userSetting
            , AudioManager.stopBGM ()
            )

        _ ->
            -- 存在しないcsvFileNameを指定した or UserSettingが読み込めなかった場合、Homeに戻す
            let
                navKey =
                    Session.toNavKey session
            in
            ( initModel session allMusicData audioLoadingS MusicData.empty UserSetting.init
            , Route.replaceUrl navKey Route.Home
            )


initModel : Session -> AllMusicData -> AudioLoadingS -> MusicData -> UserSetting -> Model
initModel session allMusicData audioLoadingS currentMusicData userSetting =
    { session = session
    , allMusicData = allMusicData
    , audioLoadingS = audioLoadingS
    , currentMusicData = currentMusicData
    , userSetting = userSetting
    , playStatus = Ready
    , allNotes = currentMusicData.allNotes
    , currentMusicTime = 0
    , score = Score.init
    , combo = Combo.init
    , lanes = List.map Lane.new allKeyStrList
    }



-- UPDATE


type Msg
    = FinishedCountdown ()
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey
    | Tick Time.Posix
    | GotCurrentMusicTime CurrentMusicTime
    | SavedRecord Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        bgm =
            BGM.fromMusicId model.currentMusicData.musicId
    in
    case msg of
        FinishedCountdown () ->
            case model.playStatus of
                StartCountdown ->
                    let
                        bgmVolume =
                            UserSetting.toSetting model.userSetting
                                |> Maybe.map .bgmVolume
                    in
                    ( { model | playStatus = Playing }, AudioManager.playBGM bgm bgmVolume )

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
                    , AudioManager.unPauseBGM bgm
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
                        Ready ->
                            ( { model | playStatus = StartCountdown }
                            , Process.sleep 1500
                                |> Task.perform (\_ -> FinishedCountdown ())
                            )

                        Playing ->
                            ( { model | playStatus = Pause }
                            , AudioManager.pauseBGM bgm
                            )

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
            ( model, AudioManager.getCurrentBGMTime bgm )

        GotCurrentMusicTime time ->
            let
                updatedTime =
                    time * 1000

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

                nextPlayStatus =
                    if model.currentMusicData.fullTime * 1000 <= model.currentMusicTime then
                        PreFinish

                    else
                        model.playStatus

                nextLanes =
                    if nextPlayStatus == PreFinish then
                        List.map Lane.allUnPress model.lanes

                    else
                        model.lanes

                saveRecordCmd =
                    Session.toUser model.session
                        |> Maybe.map
                            (\user ->
                                Record.saveRecord
                                    { uid = user.uid
                                    , csvFileName = model.currentMusicData.csvFileName
                                    , combo = Combo.toResultCombo model.combo
                                    , score = Score.unwrap model.score
                                    }
                            )
                        |> Maybe.withDefault Cmd.none
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
                , saveRecordCmd
                    |> cmdIf (nextPlayStatus == PreFinish)
                ]
            )

        SavedRecord isHighScore ->
            ( { model | playStatus = Finish isHighScore }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playStatus of
        Ready ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Playing ->
            Sub.batch
                [ Time.every 30 Tick -- ほぼ30fps
                , AudioManager.gotCurrentBGMTime GotCurrentMusicTime
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Pause ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        PreFinish ->
            Record.savedRecord SavedRecord

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


view : Model -> Html Msg
view model =
    let
        notesSpeed =
            UserSetting.toSetting model.userSetting
                |> Maybe.map .notesSpeed
                |> Maybe.withDefault notesSpeedDefault
    in
    div [ class "play_back" ]
        [ div
            [ class "play_contents" ]
            [ viewLanes model
            , viewNotes model notesSpeed
            , viewMusicInfo model.currentMusicData
            , viewDisplayCircle model.currentMusicData model
            ]
        , viewOverView model.currentMusicData model
        , div [] [ Page.viewLoaded ]
        ]


viewOverView : MusicData -> Model -> Html msg
viewOverView musicData model =
    case model.playStatus of
        Ready ->
            viewReady

        Playing ->
            text ""

        Pause ->
            viewPause

        PreFinish ->
            Page.viewLoading

        Finish isHighScore ->
            div []
                [ viewResult musicData isHighScore model
                , Page.viewLoaded
                ]

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


viewNotes : Model -> NotesSpeed -> Html msg
viewNotes model notesSpeed =
    div [ class "playCenterLine_judgeLine", id "judge_area" ]
        (List.map (Note.view model.currentMusicTime notesSpeed) model.allNotes)


viewMusicInfo : MusicData -> Html msg
viewMusicInfo musicData =
    div [ class "playTextArea_container" ]
        [ div [ class "playTextArea_bigText" ] [ text musicData.musicName ]
        , div [ class "playTextArea_smallText" ] [ text musicData.composer ]
        , div
            [ class "playTextArea_smallText" ]
            [ span [] [ text <| Mode.toString musicData.mode ]
            , span [] [ text "\u{3000}" ]
            , span [] [ text <| Level.toString musicData.level ]
            ]
        ]


viewDisplayCircle : MusicData -> Model -> Html msg
viewDisplayCircle musicData model =
    let
        rate =
            model.currentMusicTime / (musicData.fullTime * 1000)

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


viewReady : Html msg
viewReady =
    div [ class "play_overview" ]
        [ div
            [ class "playOverview_container" ]
            [ div [ class "playOverview_startText" ] [ text "READY" ]
            , div [ class "playOverview_startSubText" ] [ text "- Press Space to Start -" ]
            ]
        ]


viewPause : Html msg
viewPause =
    div [ class "play_overview" ]
        [ div
            [ class "playOverview_container" ]
            [ div [ class "playOverview_pauseText" ] [ text "PAUSE" ]
            , div [ class "playOverview_pauseSubText" ] [ text "- Press Space to UnPause -" ]
            ]
        ]


viewCountdown : Html msg
viewCountdown =
    div [ class "play_overview" ]
        [ div
            [ class "playOverview_container" ]
            [ div [ class "playOverview_cowntdownText is-first" ] []
            , div [ class "playOverview_cowntdownText is-second" ] []
            , div [ class "playOverview_cowntdownText is-third" ] []
            ]
        ]


viewResult : MusicData -> Bool -> Model -> Html msg
viewResult musicData isHighScore model =
    let
        isFullCombo =
            Combo.unwrap model.combo == musicData.maxCombo

        comboRank =
            Rank.newComboRank (Combo.toResultCombo model.combo) musicData.maxCombo

        scoreRank =
            Rank.newScoreRank (Score.unwrap model.score) musicData.maxScore

        tweetTextContent =
            tweetText
                musicData.musicName
                musicData.mode
                (Score.unwrap model.score)
                (Combo.unwrap model.combo)
    in
    div [ class "play_overview" ]
        [ div
            [ class "playOverview_container" ]
            [ div
                [ class "playOverview_contentsContainer" ]
                [ div [ class "playOverview_back" ] []
                , div [ class "playOverview_backInner" ] []
                , div [ class "playOverview_titleText" ] [ text "RESULT" ]
                , div [ class "playOverview_bigText" ] [ text musicData.musicName ]
                , div [ class "playOverview_smallText" ] [ text musicData.composer ]
                , div
                    [ class "playOverview_smallText" ]
                    [ span [] [ text <| Mode.toString musicData.mode ]
                    , span [] [ text "\u{3000}" ]
                    , span [] [ text <| Level.toString musicData.level ]
                    ]
                , div
                    [ class "playOverviewResultItem_container" ]
                    [ div [ class "playOverviewResultItem_box" ] []
                    , div [ class "playOverviewResultItem_labelText" ] [ text "COMBO" ]
                    , div [ class "playOverviewResultItem_rankText" ] [ text <| Rank.toString comboRank ]
                    , div [ class "playOverviewResultItem_effectText" ] [ text "Full Combo!!" ]
                        |> viewIf isFullCombo
                    , div
                        [ class "playOverviewResultItem_textContainer" ]
                        [ span
                            [ class "playOverviewResultItem_resultText" ]
                            [ text <| String.fromInt (Combo.toResultCombo model.combo) ]
                        , span
                            [ class "playOverviewResultItem_maxText" ]
                            [ text <| " / " ++ String.fromInt musicData.maxCombo ]
                        ]
                    , div [ class "playOverviewResultItem_line" ] []
                    ]
                , div
                    [ class "playOverviewResultItem_container" ]
                    [ div [ class "playOverviewResultItem_box" ] []
                    , div [ class "playOverviewResultItem_labelText" ] [ text "SCORE" ]
                    , div [ class "playOverviewResultItem_rankText" ] [ text <| Rank.toString scoreRank ]
                    , div [ class "playOverviewResultItem_effectText" ] [ text "High Score!!" ]
                        |> viewIf isHighScore
                    , div
                        [ class "playOverviewResultItem_textContainer" ]
                        [ span
                            [ class "playOverviewResultItem_resultText" ]
                            [ text <| String.fromInt (Score.unwrap model.score) ]
                        , span
                            [ class "playOverviewResultItem_maxText" ]
                            [ text <| " / " ++ String.fromInt musicData.maxScore ]
                        ]
                    , div [ class "playOverviewResultItem_line" ] []
                    ]
                , a
                    [ class "playOverviewResultItem_tweetBtn"
                    , href <| "http://twitter.com/intent/tweet?text=" ++ tweetTextContent
                    , target "_blank"
                    ]
                    [ text "- Tweet the Result -" ]

                -- 戻るボタンでプレイ画面に戻ることを許容する
                , a
                    [ class "playOverviewResultItem_backBtn", Route.href Route.Home ]
                    [ text "- Back to Home -" ]
                ]
            ]
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toAllMusicData : Model -> AllMusicData
toAllMusicData model =
    model.allMusicData


toAudioLoadingS : Model -> AudioLoadingS
toAudioLoadingS model =
    model.audioLoadingS
