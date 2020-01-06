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
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, href, id, src, style, target)
import Keyboard exposing (Key(..))
import OwnRecord exposing (OwnRecordDto)
import Page
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.JudgeCounter as JudgeCounter exposing (JudgeCounter)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note as Note exposing (Note)
import Page.Play.PlayingS as PlayingS exposing (PlayingS)
import Page.Play.ResultSavingS as ResultSavingS exposing (ResultSavingS)
import Page.Play.Score as Score exposing (Score)
import PublicRecord exposing (PublicRecordDto)
import Rank
import Record
import Route
import Session exposing (Session)
import Time
import UserSetting exposing (UserSetting)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (subIf, viewIf)



-- MODEL


type alias Model =
    { session : Session
    , allMusicData : AllMusicData
    , audioLoadingS : AudioLoadingS
    , currentMusicData : MusicData
    , userSetting : UserSetting
    , playingS : PlayingS
    , allNotes : List Note
    , currentMusicTime : CurrentMusicTime
    , score : Score
    , combo : Combo
    , judgeCounter : JudgeCounter
    , lanes : List Lane
    , resultSavingS : ResultSavingS
    }


init : Session -> AllMusicData -> AudioLoadingS -> Maybe CsvFileName -> Maybe UserSetting -> ( Model, Cmd Msg )
init session allMusicData audioLoadingS maybeCsvFileName maybeUserSetting =
    let
        maybeCurrentMusicData =
            maybeCsvFileName
                |> Maybe.andThen
                    (\csvFileName ->
                        AllMusicData.findByCsvFileName csvFileName allMusicData
                    )
    in
    case ( maybeCurrentMusicData, maybeUserSetting ) of
        ( Just currentMusicData, Just userSetting ) ->
            ( { session = session
              , allMusicData = allMusicData
              , audioLoadingS = audioLoadingS
              , currentMusicData = currentMusicData
              , userSetting = userSetting
              , playingS = PlayingS.init
              , allNotes = currentMusicData.allNotes
              , currentMusicTime = 0
              , score = Score.init
              , combo = Combo.init
              , judgeCounter = JudgeCounter.init
              , lanes = List.map Lane.new allKeyStrList
              , resultSavingS = ResultSavingS.init
              }
            , AudioManager.stopBGM ()
            )

        _ ->
            -- 存在しないcsvFileNameを指定した or UserSettingが読み込めなかった場合、Homeに戻す
            ( { session = session
              , allMusicData = allMusicData
              , audioLoadingS = audioLoadingS
              , currentMusicData = MusicData.empty
              , userSetting = UserSetting.init
              , playingS = PlayingS.init
              , allNotes = []
              , currentMusicTime = 0
              , score = Score.init
              , combo = Combo.init
              , judgeCounter = JudgeCounter.init
              , lanes = List.map Lane.new allKeyStrList
              , resultSavingS = ResultSavingS.init
              }
            , Route.replaceUrl (Session.toNavKey session) Route.Home
            )



-- UPDATE


type Msg
    = Tick Time.Posix
    | GotCurrentMusicTime CurrentMusicTime
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey
    | FinishedCountdown ()
    | FinishedMusic Float
    | GotCurrentOwnRecord OwnRecordDto
    | GotCurrentPublicRecord PublicRecordDto
    | SavedRecord ()
    | SavedUpdatedOwnRecord ()
    | SavedUpdatedPublicRecord ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        bgm =
            BGM.fromMusicId model.currentMusicData.musicId

        bgmVolume =
            UserSetting.toSetting model.userSetting
                |> Maybe.map .bgmVolume
    in
    case msg of
        Tick _ ->
            ( model, AudioManager.getCurrentBGMTime bgm )

        GotCurrentMusicTime time ->
            let
                updatedTime =
                    time * 1000

                updatedNotes =
                    model.allNotes
                        |> List.map (Note.update updatedTime model.lanes)

                nextAllNotes =
                    updatedNotes
                        |> List.filter (not << Note.isDisabled)

                headNotes =
                    Note.headNotes updatedNotes

                nextScore =
                    Score.update headNotes model.score

                nextCombo =
                    Combo.update headNotes model.combo

                nextJudgeCounter =
                    JudgeCounter.update headNotes model.judgeCounter
            in
            ( { model
                | currentMusicTime = updatedTime
                , allNotes = nextAllNotes
                , score = nextScore
                , combo = nextCombo
                , judgeCounter = nextJudgeCounter
              }
            , Cmd.batch
                [ headNotes
                    |> List.map (Note.headNoteJudgeEffect >> Judge.judgeEffectCmd)
                    |> Cmd.batch
                , headNotes
                    |> List.map Note.headNoteJudge
                    |> Judge.playMissEffectAnimCmd
                , Combo.comboEffectCmd model.combo nextCombo
                ]
            )

        KeyDown rawKey ->
            let
                maybeKey =
                    Keyboard.anyKeyUpper rawKey
            in
            case maybeKey of
                Just Keyboard.Spacebar ->
                    let
                        ( nextPlayingS, playingSCmd ) =
                            PlayingS.pressSpaceKey bgm (FinishedCountdown ()) model.playingS
                    in
                    ( { model | playingS = nextPlayingS }, playingSCmd )

                Just (Keyboard.Character keyStr) ->
                    let
                        nextLanes =
                            List.map (Lane.press keyStr) model.lanes
                    in
                    if not <| PlayingS.isPlaying model.playingS then
                        -- Playingの時しか判定しない
                        -- LaneのisPressingの更新のみ行う
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if List.any (Lane.isPressing keyStr) model.lanes then
                        -- すでにそのレーンのキーが押されている状態ではKeyDown判定しない
                        ( model, Cmd.none )

                    else
                        let
                            maybeHeadNote =
                                Note.maybeHeadNote keyStr model.allNotes
                        in
                        case maybeHeadNote of
                            Just headNote ->
                                let
                                    judge =
                                        Judge.judgeKeyDown model.currentMusicTime (Note.toJustTime headNote)

                                    nextAllNotes =
                                        model.allNotes
                                            |> Note.updateHeadNote keyStr (Note.updateKeyDown judge)
                                            |> List.filter (not << Note.isDisabled)

                                    nextScore =
                                        Score.updateKeyDown judge model.score

                                    nextCombo =
                                        Combo.updateKeyDown judge model.combo

                                    nextJudgeCounter =
                                        JudgeCounter.updateKeyDown judge model.judgeCounter
                                in
                                ( { model
                                    | allNotes = nextAllNotes
                                    , lanes = nextLanes
                                    , score = nextScore
                                    , combo = nextCombo
                                    , judgeCounter = nextJudgeCounter
                                  }
                                , Cmd.batch
                                    [ Judge.judgeEffectCmd
                                        { keyStr = keyStr
                                        , judge = judge
                                        , isLongNote = Note.isLongNote headNote
                                        }
                                    , Combo.comboEffectCmd model.combo nextCombo
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
                    if not <| PlayingS.isPlaying model.playingS then
                        -- Playingの時しか判定しない
                        -- LaneのisPressingの更新のみ行う
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if not <| List.any (Lane.isPressing keyStr) model.lanes then
                        -- もうそのレーンのキーが押されていない状態ではKeyUp判定しない
                        ( model, Cmd.none )

                    else
                        let
                            maybeHeadNote =
                                Note.maybeHeadNote keyStr model.allNotes
                        in
                        case maybeHeadNote of
                            Just _ ->
                                let
                                    nextAllNotes =
                                        model.allNotes
                                            |> Note.updateHeadNote keyStr Note.updateKeyUp
                                            |> List.filter (not << Note.isDisabled)
                                in
                                ( { model | allNotes = nextAllNotes, lanes = nextLanes }, Cmd.none )

                            Nothing ->
                                -- このレーンのノーツはもうない
                                ( { model | lanes = nextLanes }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FinishedCountdown () ->
            let
                ( nextPlayingS, playingSCmd ) =
                    PlayingS.finishedCountdown bgm bgmVolume model.playingS
            in
            ( { model | playingS = nextPlayingS }, playingSCmd )

        FinishedMusic time ->
            case Session.toUser model.session of
                Just user ->
                    let
                        record =
                            Record.new
                                { uid = user.uid
                                , csvFileName = model.currentMusicData.csvFileName
                                , combo = Combo.toResultCombo model.combo
                                , score = Score.unwrap model.score
                                , createdAt = time
                                }

                        ( nextResultSavingS, resultSavingSCmd ) =
                            ResultSavingS.startSaving record model.resultSavingS
                    in
                    ( { model
                        | playingS = PlayingS.finish
                        , lanes = List.map Lane.allUnPress model.lanes
                        , resultSavingS = nextResultSavingS
                      }
                    , resultSavingSCmd
                    )

                Nothing ->
                    -- リザルト保存時にuserがなかったらエラー画面に飛ばす
                    ( model, Route.replaceUrl (Session.toNavKey model.session) Route.Error )

        GotCurrentOwnRecord ownRecordDto ->
            let
                ( nextResultSavingS, resultSavingSCmd ) =
                    ResultSavingS.gotCurrentOwnRecord ownRecordDto model.resultSavingS
            in
            ( { model | resultSavingS = nextResultSavingS }, resultSavingSCmd )

        GotCurrentPublicRecord publicRecordDto ->
            let
                ( nextResultSavingS, resultSavingSCmd ) =
                    ResultSavingS.gotCurrentPublicRecord publicRecordDto model.resultSavingS
            in
            ( { model | resultSavingS = nextResultSavingS }, resultSavingSCmd )

        SavedRecord _ ->
            ( { model | resultSavingS = ResultSavingS.savedRecord model.resultSavingS }
            , Cmd.none
            )

        SavedUpdatedOwnRecord _ ->
            ( { model | resultSavingS = ResultSavingS.savedUpdatedOwnRecord model.resultSavingS }
            , Cmd.none
            )

        SavedUpdatedPublicRecord _ ->
            ( { model | resultSavingS = ResultSavingS.savedUpdatedPublicRecord model.resultSavingS }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        updateCurrentMusicTimeSub =
            Sub.batch
                [ Time.every 30 Tick -- ほぼ30fps
                , AudioManager.gotCurrentBGMTime GotCurrentMusicTime
                ]

        keyboardSub =
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        saveResultSub =
            Sub.batch
                [ Record.savedRecord SavedRecord
                , OwnRecord.gotOwnRecord GotCurrentOwnRecord
                , OwnRecord.savedOwnRecord SavedUpdatedOwnRecord
                , PublicRecord.gotPublicRecord GotCurrentPublicRecord
                , PublicRecord.savedPublicRecord SavedUpdatedPublicRecord
                ]
    in
    Sub.batch
        [ AudioManager.onEndBGM FinishedMusic
        , updateCurrentMusicTimeSub
            |> subIf (PlayingS.isPlaying model.playingS)
        , keyboardSub
            |> subIf (not <| PlayingS.isFinish model.playingS)
        , saveResultSub
            |> subIf (PlayingS.isFinish model.playingS)
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
            [ viewLanes model.lanes
            , viewNotes model.currentMusicTime notesSpeed model.allNotes
            , viewMusicInfo model.currentMusicData
            , viewDisplayCircle model.currentMusicData model.currentMusicTime model.combo model.score
            ]
        , div [ class "playJudgeEffect_missEffect", id "missEffect" ] []
        , viewReady
            |> viewIf (PlayingS.isReady model.playingS)
        , viewPause
            |> viewIf (PlayingS.isPause model.playingS)
        , viewCountdown
            |> viewIf (PlayingS.isCountdown model.playingS)
        , viewResult model.currentMusicData model.resultSavingS model.judgeCounter
            |> viewIf (PlayingS.isFinish model.playingS)
        , div [] [ Page.viewLoaded ]
        ]


viewLanes : List Lane -> Html msg
viewLanes lanes =
    div []
        [ div [ class "playCenterLine_judgeAreaLine outer center" ] []
        , div [ class "playCenterLine_judgeAreaLine outer left" ] []
        , div [ class "playCenterLine_judgeAreaLine outer right" ] []
        , div [] (List.map Lane.view lanes)
        ]


viewNotes : CurrentMusicTime -> NotesSpeed -> List Note -> Html msg
viewNotes currentMusicTime notesSpeed allNotes =
    div [ class "playCenterLine_judgeLine", id "judge_area" ]
        (List.map (Note.view currentMusicTime notesSpeed) allNotes)


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


viewDisplayCircle : MusicData -> CurrentMusicTime -> Combo -> Score -> Html msg
viewDisplayCircle musicData currentMusicTime combo score =
    let
        rate =
            currentMusicTime / (musicData.fullTime * 1000)

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
            , div [ class "playDisplay_scoreText" ] [ text <| String.fromInt (Score.unwrap score) ]
            , div [ class "playDisplay_comboLabelText" ] [ text "- COMBO -" ]
            , div [ class "playDisplay_comboText", id "comboText" ] [ text <| String.fromInt (Combo.unwrap combo) ]
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


viewResult : MusicData -> ResultSavingS -> JudgeCounter -> Html msg
viewResult musicData resultSavingS judgeCounter =
    case ResultSavingS.toResult resultSavingS of
        Just { record, isBestCombo, isBestScore } ->
            let
                isFullCombo =
                    record.combo == musicData.maxCombo

                comboRank =
                    Rank.new record.combo musicData.maxCombo

                scoreRank =
                    Rank.new record.score musicData.maxScore

                tweetTextContent =
                    tweetText musicData.musicName musicData.mode record.combo record.score

                viewNotesDetail judge count =
                    div [ class "playResultNotesDetail_container" ]
                        [ div [ class "playResultNotesDetail_box" ] []
                        , div [ class "playResultNotesDetail_labelText" ] [ text <| Judge.toString judge ]
                        , div [ class "playResultNotesDetail_numText" ] [ text <| String.fromInt count ]
                        ]

                viewComboEffectText =
                    div [ class "playResultItem_effectText" ]
                        [ text "自己ベスト更新！"
                            |> viewIf isBestCombo
                        , text "フルコンボ！"
                            |> viewIf isFullCombo
                        ]

                viewScoreEffectText =
                    div [ class "playResultItem_effectText" ]
                        [ text "自己ベスト更新！"
                            |> viewIf isBestScore
                        ]

                viewResultItem labelText rank viewEffectText result max =
                    div [ class "playResultItem_container" ]
                        [ div [ class "playResultItem_box" ] []
                        , div [ class "playResultItem_labelText" ] [ text labelText ]
                        , div [ class "playResultItem_rankText" ] [ text <| Rank.toString rank ]
                        , viewEffectText
                        , div
                            [ class "playResultItem_textContainer" ]
                            [ span [ class "playResultItem_resultText" ] [ text <| String.fromInt result ]
                            , span [ class "playResultItem_maxText" ] [ text <| " / " ++ String.fromInt max ]
                            ]
                        , div [ class "playResultItem_line" ] []
                        ]
            in
            div [ class "play_overview" ]
                [ div
                    [ class "playOverview_container" ]
                    [ div
                        [ class "playResult_contentsContainer" ]
                        [ div [ class "playResult_back" ] []
                        , div [ class "playResult_backInner" ] []
                        , div [ class "playResult_titleText" ] [ text "RESULT" ]
                        , div [ class "playResult_bigText" ] [ text musicData.musicName ]
                        , div [ class "playResult_smallText" ] [ text musicData.composer ]
                        , div
                            [ class "playResult_smallText" ]
                            [ span [] [ text <| Mode.toString musicData.mode ]
                            , span [] [ text "\u{3000}" ]
                            , span [] [ text <| Level.toString musicData.level ]
                            ]
                        , div
                            [ class "playResultNotesDetails_container" ]
                            [ viewNotesDetail Perfect (JudgeCounter.toPerfect judgeCounter)
                            , viewNotesDetail Great (JudgeCounter.toGreat judgeCounter)
                            , viewNotesDetail Good (JudgeCounter.toGood judgeCounter)
                            , viewNotesDetail Lost (JudgeCounter.toLost judgeCounter)
                            , viewNotesDetail Miss (JudgeCounter.toMiss judgeCounter)
                            ]
                        , viewResultItem "COMBO" comboRank viewComboEffectText record.combo musicData.maxCombo
                        , viewResultItem "SCORE" scoreRank viewScoreEffectText record.score musicData.maxScore

                        -- 戻るボタンでプレイ画面に戻ることを許容する
                        , a
                            [ class "playResult_backBtn", Route.href Route.Home ]
                            [ text "- Back to Home -" ]
                        ]
                    , a
                        [ class "playResult_tweetBtnContainer"
                        , href <| "http://twitter.com/intent/tweet?text=" ++ tweetTextContent
                        , target "_blank"
                        ]
                        [ img [ class "playResult_tweetBtnBack", src "./img/icon_fukidashi2.png" ] []
                        , img [ class "playResult_tweetBtnIcon", src "./img/icon_twitter_blue.png" ] []
                        ]
                    ]
                ]

        Nothing ->
            div [ class "play_overview" ] [ Page.viewLoading ]



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
