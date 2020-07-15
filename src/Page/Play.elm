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
import AudioManager.SE as SE
import Constants exposing (allKeyList, notesSpeedDefault, tweetText)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, href, id, src, style, target)
import Html.Events exposing (onMouseUp)
import Keyboard exposing (Key(..))
import OwnRecord exposing (OwnRecordDto)
import Page
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Guideline as Guideline exposing (Guideline)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.JudgeCounter as JudgeCounter exposing (JudgeCounter)
import Page.Play.Key as Key
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note as Note exposing (Note)
import Page.Play.PlayingS as PlayingS exposing (PlayingS)
import Page.Play.Result as Result exposing (Result)
import Page.Play.ResultSavingS as ResultSavingS exposing (ResultSavingS)
import Page.Play.Score as Score exposing (Score)
import PublicRecord exposing (PublicRecordDto)
import Rank
import Route
import Session exposing (Session)
import Session.User as User
import Time
import Tracking
import UserSetting exposing (UserSetting)
import UserSetting.Setting as Setting exposing (Setting)
import UserSetting.Setting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (cmdIf, subIf, viewIf)



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
    , guidelines : List Guideline
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
              , lanes = List.map Lane.new allKeyList
              , guidelines =
                    Guideline.createGuidelines
                        { bpm = currentMusicData.bpm
                        , beatsCountPerMeasure = currentMusicData.beatsCountPerMeasure
                        , offset = currentMusicData.offset
                        , fullTime = currentMusicData.fullTime
                        }
              , resultSavingS = ResultSavingS.init
              }
            , Cmd.batch
                [ AudioManager.stopBGM ()
                , Tracking.trackingPlayStart currentMusicData.csvFileName
                ]
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
              , lanes = List.map Lane.new allKeyList
              , guidelines = []
              , resultSavingS = ResultSavingS.init
              }
            , Cmd.batch
                [ AudioManager.stopBGM ()
                , Route.replaceUrl (Session.toNavKey session) Route.Home
                ]
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
    | SavedResult ()
    | SavedUpdatedOwnRecord ()
    | SavedUpdatedPublicRecord ()
    | PlayTweetBtnSE
    | PlayBackBtnSE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        bgm =
            BGM.fromMusicId model.currentMusicData.musicId

        bgmVolume =
            model.userSetting
                |> UserSetting.toSetting
                |> Maybe.map Setting.toBgmVolume

        seVolume =
            model.userSetting
                |> UserSetting.toSetting
                |> Maybe.map Setting.toSeVolume
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

                nextGuidelines =
                    model.guidelines
                        |> List.map (Guideline.update updatedTime)
            in
            ( { model
                | currentMusicTime = updatedTime
                , allNotes = nextAllNotes
                , score = nextScore
                , combo = nextCombo
                , judgeCounter = nextJudgeCounter
                , guidelines = nextGuidelines
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
            case Key.new rawKey of
                Key.Character key ->
                    let
                        nextLanes =
                            List.map (Lane.press key) model.lanes
                    in
                    if not <| PlayingS.isPlaying model.playingS then
                        -- Playingの時しか判定しない
                        -- LaneのisPressingの更新のみ行う
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if List.any (Lane.isPressing key) model.lanes then
                        -- すでにそのレーンのキーが押されている状態ではKeyDown判定しない
                        ( model, Cmd.none )

                    else
                        let
                            maybeHeadNote =
                                Note.maybeHeadNote key model.allNotes
                        in
                        case maybeHeadNote of
                            Just headNote ->
                                let
                                    judge =
                                        Judge.judgeKeyDown model.currentMusicTime (Note.toJustTime headNote)

                                    nextAllNotes =
                                        model.allNotes
                                            |> Note.updateHeadNote key (Note.updateKeyDown judge)
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
                                        { key = key
                                        , judge = judge
                                        , isLongNote = Note.isLongNote headNote
                                        }
                                    , Combo.comboEffectCmd model.combo nextCombo
                                    ]
                                )

                            Nothing ->
                                -- このレーンのノーツはもうない
                                ( { model | lanes = nextLanes }, Cmd.none )

                Key.Space ->
                    let
                        ( nextPlayingS, playingSCmd ) =
                            PlayingS.pressSpaceKey bgm seVolume (FinishedCountdown ()) model.playingS
                    in
                    ( { model | playingS = nextPlayingS }, playingSCmd )

                Key.Invalid ->
                    ( model, Cmd.none )

        KeyUp rawKey ->
            case Key.new rawKey of
                Key.Character key ->
                    let
                        nextLanes =
                            List.map (Lane.unPress key) model.lanes
                    in
                    if not <| PlayingS.isPlaying model.playingS then
                        -- Playingの時しか判定しない
                        -- LaneのisPressingの更新のみ行う
                        ( { model | lanes = nextLanes }, Cmd.none )

                    else if not <| List.any (Lane.isPressing key) model.lanes then
                        -- もうそのレーンのキーが押されていない状態ではKeyUp判定しない
                        ( model, Cmd.none )

                    else
                        let
                            maybeHeadNote =
                                Note.maybeHeadNote key model.allNotes
                        in
                        case maybeHeadNote of
                            Just _ ->
                                let
                                    nextAllNotes =
                                        model.allNotes
                                            |> Note.updateHeadNote key Note.updateKeyUp
                                            |> List.filter (not << Note.isDisabled)
                                in
                                ( { model | allNotes = nextAllNotes, lanes = nextLanes }, Cmd.none )

                            Nothing ->
                                -- このレーンのノーツはもうない
                                ( { model | lanes = nextLanes }, Cmd.none )

                Key.Space ->
                    ( model, Cmd.none )

                Key.Invalid ->
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
                        result =
                            Result.new
                                { uid = User.toUid user
                                , csvFileName = model.currentMusicData.csvFileName
                                , combo = Combo.toResultCombo model.combo
                                , score = Score.unwrap model.score
                                , createdAt = time
                                }
                                model.currentMusicData.maxCombo

                        ( nextResultSavingS, resultSavingSCmd ) =
                            ResultSavingS.startSaving result model.resultSavingS
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

        SavedResult _ ->
            let
                nextResultSavingS =
                    ResultSavingS.savedResult model.resultSavingS
            in
            ( { model | resultSavingS = nextResultSavingS }
            , Cmd.batch
                [ AudioManager.playSE SE.Result seVolume
                , Tracking.trackingPlayEnd model.currentMusicData.csvFileName
                ]
                |> cmdIf (not <| ResultSavingS.toResult nextResultSavingS == Nothing)
            )

        SavedUpdatedOwnRecord _ ->
            let
                nextResultSavingS =
                    ResultSavingS.savedUpdatedOwnRecord model.resultSavingS
            in
            ( { model | resultSavingS = nextResultSavingS }
            , Cmd.batch
                [ AudioManager.playSE SE.Result seVolume
                , Tracking.trackingPlayEnd model.currentMusicData.csvFileName
                ]
                |> cmdIf (not <| ResultSavingS.toResult nextResultSavingS == Nothing)
            )

        SavedUpdatedPublicRecord _ ->
            let
                nextResultSavingS =
                    ResultSavingS.savedUpdatedPublicRecord model.resultSavingS
            in
            ( { model | resultSavingS = nextResultSavingS }
            , Cmd.batch
                [ AudioManager.playSE SE.Result seVolume
                , Tracking.trackingPlayEnd model.currentMusicData.csvFileName
                ]
                |> cmdIf (not <| ResultSavingS.toResult nextResultSavingS == Nothing)
            )

        PlayTweetBtnSE ->
            ( model, AudioManager.playSE SE.Select seVolume )

        PlayBackBtnSE ->
            ( model, AudioManager.playSE SE.Cancel seVolume )



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
                [ Result.savedResult SavedResult
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
                |> Maybe.map Setting.toNotesSpeed
                |> Maybe.withDefault notesSpeedDefault
    in
    div [ class "play_back" ]
        [ div
            [ class "play_contents" ]
            [ viewLanes model.lanes
            , viewGuidelines model.currentMusicTime notesSpeed model.guidelines
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
    div [ class "playCenterLine_judgeLine" ]
        (List.map (Note.view currentMusicTime notesSpeed) allNotes)


viewGuidelines : CurrentMusicTime -> NotesSpeed -> List Guideline -> Html msg
viewGuidelines currentMusicTime notesSpeed guidelines =
    div [ class "playCenterLine_judgeLine" ]
        (List.map (Guideline.view currentMusicTime notesSpeed) guidelines)


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


viewResult : MusicData -> ResultSavingS -> JudgeCounter -> Html Msg
viewResult musicData resultSavingS judgeCounter =
    case ResultSavingS.toResult resultSavingS of
        Just result ->
            let
                comboRank =
                    Rank.newComboRank (Result.toCombo result) musicData.maxCombo

                scoreRank =
                    Rank.newScoreRank (Result.toScore result) musicData.maxScore

                tweetTextContent =
                    tweetText
                        musicData.musicName
                        musicData.mode
                        (Result.toCombo result)
                        (Result.toScore result)

                viewNotesDetail judge count =
                    div [ class "playResultNotesDetail_container" ]
                        [ div [ class "playResultNotesDetail_box" ] []
                        , div [ class "playResultNotesDetail_labelText" ] [ text <| Judge.toString judge ]
                        , div [ class "playResultNotesDetail_numText" ] [ text <| String.fromInt count ]
                        ]

                viewComboEffectText =
                    div [ class "playResultItem_effectText" ]
                        [ text "自己ベスト更新！"
                            |> viewIf (Result.isBestCombo result)
                        , text "フルコンボ！"
                            |> viewIf (Result.isFullCombo result)
                        ]

                viewScoreEffectText =
                    div [ class "playResultItem_effectText" ]
                        [ text "自己ベスト更新！"
                            |> viewIf (Result.isBestScore result)
                        ]

                viewResultItem labelText rank viewEffectText result_ max =
                    div [ class "playResultItem_container" ]
                        [ div [ class "playResultItem_box" ] []
                        , div [ class "playResultItem_labelText" ] [ text labelText ]
                        , div [ class "playResultItem_rankText" ] [ text <| Rank.toString rank ]
                        , viewEffectText
                        , div
                            [ class "playResultItem_textContainer" ]
                            [ span [ class "playResultItem_resultText" ] [ text <| String.fromInt result_ ]
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
                        [ a
                            [ class "playResult_tweetBtnContainer"
                            , href <| "http://twitter.com/intent/tweet?text=" ++ tweetTextContent
                            , target "_blank"
                            , onMouseUp PlayTweetBtnSE
                            ]
                            [ img [ class "playResult_tweetBtnBack", src "./img/icon_fukidashi.png" ] []
                            , img [ class "playResult_tweetBtnIcon", src "./img/icon_twitter_blue.png" ] []
                            ]
                        , div [ class "playResult_back" ] []
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
                            , viewNotesDetail Nice (JudgeCounter.toNice judgeCounter)
                            , viewNotesDetail Good (JudgeCounter.toGood judgeCounter)
                            , viewNotesDetail Lost (JudgeCounter.toLost judgeCounter)
                            , viewNotesDetail Miss (JudgeCounter.toMiss judgeCounter)
                            ]
                        , viewResultItem "COMBO" comboRank viewComboEffectText (Result.toCombo result) musicData.maxCombo
                        , viewResultItem "SCORE" scoreRank viewScoreEffectText (Result.toScore result) musicData.maxScore

                        -- 戻るボタンでプレイ画面に戻ることを許容する
                        , a
                            [ class "playResult_backBtn", Route.href Route.Home, onMouseUp PlayBackBtnSE ]
                            [ text "- Back to Home -" ]
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
