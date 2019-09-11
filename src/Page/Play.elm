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


init : Session -> CsvFileName -> ( Model, Cmd Msg )
init session csvFileName =
    let
        _ =
            Debug.log "csvFileName" csvFileName
    in
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
    = Tick Time.Posix
    | GotCurrentMusicTime CurrentMusicTime
    | GotPlayingMusicInfo MusicInfoDto
    | GotAllNotes AllNotesDto
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            in
            ( { model
                | currentMusicTime = updatedTime
                , playStatus = nextPlayStatus
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

        KeyDown rawKey ->
            let
                maybeKey =
                    Keyboard.anyKeyUpper rawKey
            in
            case maybeKey of
                Just Keyboard.Spacebar ->
                    let
                        ( nextPlayStatus, cmd ) =
                            case model.playStatus of
                                NotStart ->
                                    if PlayingMusicInfo.isLoaded model.playingMusicInfo then
                                        ( Playing, startMusic () )

                                    else
                                        ( NotStart, startMusic () )

                                Playing ->
                                    ( Pause, pauseMusic () )

                                Pause ->
                                    ( Playing, unPauseMusic () )

                                Finish ->
                                    ( Finish, Cmd.none )
                    in
                    ( { model | playStatus = nextPlayStatus }, cmd )

                Just (Keyboard.Character keyStr) ->
                    let
                        notesPerLane =
                            model.allNotes
                                |> AllNotes.toNotesPerLane keyStr
                    in
                    -- すでにそのレーンのキーが押されている状態ではKeyDown判定しない
                    if Lanes.isPressing keyStr model.lanes then
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
                    let
                        nextAllNotes =
                            model.allNotes
                                |> AllNotes.updateNotesKeyUp keyStr

                        nextLanes =
                            Lanes.updateKeyUp keyStr model.lanes
                    in
                    ( { model
                        | allNotes = nextAllNotes
                        , lanes = nextLanes
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



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
                        , AllNotes.view model.currentMusicTime model.speed model.allNotes
                        , viewDisplayCircle model
                        , div [ class "playTextArea1_container" ]
                            [ div
                                [ class "playTextArea1_bigText" ]
                                [ text <| (MusicInfo.toMusicName <| PlayingMusicInfo.toMusicInfo model.playingMusicInfo) ]
                            , div
                                [ class "playTextArea1_smallText" ]
                                [ text <| (MusicInfo.toComposer <| PlayingMusicInfo.toMusicInfo model.playingMusicInfo) ]
                            , div
                                [ class "playTextArea1_smallText" ]
                                [ text <|
                                    ((Mode.unwrap <| MusicInfo.toMode <| PlayingMusicInfo.toMusicInfo model.playingMusicInfo)
                                        ++ "\u{3000}"
                                        ++ (MusicInfo.toStringLevel <| PlayingMusicInfo.toMusicInfo model.playingMusicInfo)
                                    )
                                ]
                            ]
                        , div [ class "playTextArea2_container" ]
                            [ div [] [ text "Spaceキーでスタート" ]
                                |> Page.viewIf (model.playStatus == NotStart)
                            , div [] [ text "Finish!" ]
                                |> Page.viewIf (model.playStatus == Finish)
                            , a [ Route.href Route.Home ] [ text "Homeに戻る" ]
                                |> Page.viewIf (model.playStatus == Finish)
                            ]
                        ]
                    ]
                ]

        else
            text ""
    }


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
        , div
            [ class "half1"
            , style "transform" half1Rotate
            ]
            []
        , div
            [ class "half2"
            , style "transform" half2Rotate
            , style "background-color" half2Color
            ]
            []
        , div [ class "playDisplay_centerTextArea" ]
            [ div [ class "playDisplay_scoreLabelText" ] [ text "- SCORE -" ]
            , div [ class "playDisplay_scoreText" ] [ text <| Score.toString model.score ]
            , div [ class "playDisplay_comboLabelText" ] [ text "- COMBO -" ]
            , div [ class "playDisplay_comboText", id "comboText" ] [ text <| Combo.toString model.combo ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playStatus of
        Playing ->
            Sub.batch
                [ Time.every 10 Tick
                , gotCurrentMusicTime GotCurrentMusicTime
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        _ ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                , gotAllNotes GotAllNotes
                , gotPlayingMusicInfo GotPlayingMusicInfo
                ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
