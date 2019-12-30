port module Page.Play exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toAllMusicInfoList
    , toSession
    , toUserSetting
    , update
    , view
    )

import AllMusicInfoList exposing (AllMusicInfoList)
import AudioManager
import AudioManager.AudioInfo as AudioInfo exposing (AudioInfoDto)
import Constants exposing (allKeyStr, tweetText)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, id, style, target)
import Keyboard exposing (Key(..))
import MusicInfo exposing (MusicInfo)
import MusicInfo.CsvFileName as CsvFileName exposing (CsvFileName)
import MusicInfo.Level as Level
import MusicInfo.Mode as Mode
import Page
import Page.Play.AudioLoadingS as AudioLoadingS exposing (AudioLoadingS)
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime, update)
import Page.Play.Judge as Judge exposing (Judge(..))
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Note as Note exposing (Note, NoteDto)
import Page.Play.Score as Score exposing (Score)
import Process
import Rank
import Record exposing (RecordDto)
import Route
import Session exposing (Session)
import Set
import Setting.NotesSpeed exposing (NotesSpeed)
import Task
import Time
import UserSetting exposing (UserSetting)
import Utils exposing (cmdIf, viewIf)



-- MODEL


type alias Model =
    { session : Session
    , userSetting : UserSetting
    , allMusicInfoList : AllMusicInfoList
    , currentMusicInfo : MusicInfo
    , playStatus : PlayStatus
    , allNotes : List Note
    , notesSpeed : NotesSpeed
    , currentMusicTime : CurrentMusicTime
    , score : Score
    , combo : Combo
    , lanes : List Lane
    , audioLoadingS : AudioLoadingS
    }


type PlayStatus
    = Loading
    | Ready
    | Playing
    | Pause
    | PreFinish
    | Finish Bool
    | StartCountdown
    | PauseCountdown


init : Session -> UserSetting -> AllMusicInfoList -> CsvFileName -> ( Model, Cmd Msg )
init session userSetting allMusicInfoList csvFileName =
    let
        maybeCurrentMusicInfo =
            AllMusicInfoList.toMusicInfoFindByCsvFileName csvFileName allMusicInfoList

        maybeNotesSpeed =
            userSetting
                |> UserSetting.toSetting
                |> Maybe.map .notesSpeed
    in
    case ( maybeCurrentMusicInfo, maybeNotesSpeed ) of
        ( Just currentMusicInfo, Just notesSpeed ) ->
            ( initModel session userSetting allMusicInfoList currentMusicInfo notesSpeed
            , Cmd.batch
                [ getAllNotes
                    { csvFileName = csvFileName
                    , bpm = currentMusicInfo.bpm
                    , beatsCountPerMeasure = currentMusicInfo.beatsCountPerMeasure
                    , offset = currentMusicInfo.offset
                    }
                , AudioManager.getAudioInfo (CsvFileName.toMusicId csvFileName)
                ]
            )

        _ ->
            -- 存在しないcsvFileNameを指定した or maybeNotesSpeed == Nothing だった場合、Homeに戻す
            let
                navKey =
                    Session.toNavKey session
            in
            ( initModel session userSetting allMusicInfoList MusicInfo.empty 0
            , Route.replaceUrl navKey Route.Home
            )


initModel : Session -> UserSetting -> AllMusicInfoList -> MusicInfo -> NotesSpeed -> Model
initModel session userSetting allMusicInfoList currentMusicInfo notesSpeed =
    { session = session
    , userSetting = userSetting
    , allMusicInfoList = allMusicInfoList
    , currentMusicInfo = currentMusicInfo
    , playStatus = Loading
    , allNotes = []
    , notesSpeed = notesSpeed
    , currentMusicTime = 0
    , score = Score.init
    , combo = Combo.init
    , lanes = List.map Lane.new allKeyStr
    , audioLoadingS = AudioLoadingS.init
    }



-- UPDATE


type Msg
    = GotAllNotes (List NoteDto)
    | GotAudioInfo AudioInfoDto
    | FinishedCountdown ()
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey
    | Tick Time.Posix
    | GotCurrentMusicTime CurrentMusicTime
    | SavedRecord Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllNotes noteDtos ->
            let
                allNotes =
                    noteDtos
                        -- ここで無効なノーツをはじいておく
                        |> List.filter (\noteDto -> noteDto.longTime >= 0)
                        |> List.map Note.new

                nextPlayStatus =
                    if AudioLoadingS.isLoaded model.audioLoadingS then
                        Ready

                    else
                        Loading
            in
            ( { model | allNotes = allNotes, playStatus = nextPlayStatus }, Cmd.none )

        GotAudioInfo audioInfoDto ->
            let
                audioInfo =
                    AudioInfo.new audioInfoDto

                nextPlayStatus =
                    if not <| List.isEmpty model.allNotes then
                        Ready

                    else
                        Loading
            in
            ( { model
                | audioLoadingS = AudioLoadingS.loaded audioInfo.audioUrl
                , playStatus = nextPlayStatus
              }
            , Cmd.none
            )

        FinishedCountdown () ->
            case model.playStatus of
                StartCountdown ->
                    let
                        audioUrl =
                            AudioLoadingS.toAudioUrl model.audioLoadingS

                        bgmVolume =
                            UserSetting.toSetting model.userSetting
                                |> Maybe.map .bgmVolume
                    in
                    ( { model | playStatus = Playing }
                    , AudioManager.playBGM audioUrl bgmVolume False
                    )

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
                    , AudioManager.unPauseBGM ()
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
                            , AudioManager.pauseBGM ()
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

                nextPlayStatus =
                    if model.currentMusicInfo.fullTime * 1000 <= model.currentMusicTime then
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
                                saveRecord
                                    { uid = user.uid
                                    , csvFileName = model.currentMusicInfo.csvFileName
                                    , combo = Combo.toMaxCombo model.combo
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



-- PORT


port getAllNotes : { csvFileName : String, bpm : Int, beatsCountPerMeasure : Int, offset : Float } -> Cmd msg


port gotAllNotes : (List NoteDto -> msg) -> Sub msg


port getCurrentMusicTime : () -> Cmd msg


port gotCurrentMusicTime : (Float -> msg) -> Sub msg


port saveRecord : RecordDto -> Cmd msg


port savedRecord : (Bool -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playStatus of
        Loading ->
            Sub.batch
                [ gotAllNotes GotAllNotes
                , AudioManager.gotAudioInfo GotAudioInfo
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Ready ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Playing ->
            Sub.batch
                [ Time.every 30 Tick -- ほぼ30fps
                , gotCurrentMusicTime GotCurrentMusicTime
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        Pause ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        PreFinish ->
            savedRecord SavedRecord

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
    if not <| model.playStatus == Loading then
        div [ class "play_contentsContainer" ]
            [ div
                [ class "play_contents" ]
                [ viewLanes model
                , viewNotes model model.notesSpeed
                , viewMusicInfo model.currentMusicInfo
                , viewDisplayCircle model.currentMusicInfo model
                ]
            , viewOverView model.currentMusicInfo model
            , div [] [ Page.viewLoaded ]
            ]

    else
        div [ class "play_contentsContainer" ] [ Page.viewLoading ]


viewOverView : MusicInfo -> Model -> Html msg
viewOverView musicInfo model =
    case model.playStatus of
        Loading ->
            text ""

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
                [ viewResult musicInfo isHighScore model
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


viewResult : MusicInfo -> Bool -> Model -> Html msg
viewResult musicInfo isHighScore model =
    let
        isFullCombo =
            Combo.unwrap model.combo == musicInfo.maxCombo

        comboRank =
            Rank.newComboRank (Combo.toMaxCombo model.combo) musicInfo.maxCombo

        scoreRank =
            Rank.newScoreRank (Score.unwrap model.score) musicInfo.maxScore

        tweetTextContent =
            tweetText
                musicInfo.musicName
                musicInfo.mode
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
                        |> viewIf isFullCombo
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
                        |> viewIf isHighScore
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


toUserSetting : Model -> UserSetting
toUserSetting model =
    model.userSetting


toAllMusicInfoList : Model -> AllMusicInfoList
toAllMusicInfoList model =
    model.allMusicInfoList
