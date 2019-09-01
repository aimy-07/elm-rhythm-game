port module Page.Play exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Page
import Page.Play.AllNotes as AllNotes exposing (AllNotes)
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime, updateCurrentMusicTime)
import Page.Play.JudgeEffect as JudgeEffect exposing (JudgeEffect)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.Lane as Lane exposing (Lane)
import Page.Play.Lanes as Lanes exposing (Lanes)
import Page.Play.LongNoteLine as LongNoteLine exposing (EndTime, LongNoteLine)
import Page.Play.MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import Page.Play.Note as Note exposing (JustTime, Note)
import Page.Play.NotesPerLane as NotesPerLane exposing (NotesPerLane)
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
    , musicInfo : MusicInfo
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


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , playStatus = NotStart
      , musicInfo = MusicInfo.init
      , allNotes = AllNotes.init
      , lanes = Lanes.init
      , currentMusicTime = 0
      , speed = 0.2
      , score = Score.init
      , combo = Combo.init
      }
    , getMusicInfo ()
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | GotMusicInfo MusicInfoDto
    | KeyDown Keyboard.RawKey
    | KeyUp Keyboard.RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                updatedTime =
                    updateCurrentMusicTime model.currentMusicTime

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

                nextPlayStatus =
                    model.playStatus
                        |> Page.updateIf
                            (MusicInfo.toFullTime model.musicInfo < model.currentMusicTime)
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
                ]
            )

        GotMusicInfo rawMusicInfo ->
            let
                nextMusicInfo =
                    MusicInfo.create rawMusicInfo

                nextAllNotes =
                    MusicInfo.toAllNotes nextMusicInfo

                _ =
                    Debug.log "musicInfo" nextMusicInfo
            in
            ( { model
                | musicInfo = nextMusicInfo
                , allNotes = nextAllNotes
              }
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
                                    if MusicInfo.isLoaded model.musicInfo then
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

                            nextLanes =
                                Lanes.updateKeyDown keyStr model.lanes
                        in
                        ( { model
                            | allNotes = nextAllNotes
                            , lanes = nextLanes
                            , score = Score.add judgeKind model.score
                            , combo = Combo.update judgeKind model.combo
                          }
                        , judgeEffectCmd
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
        if MusicInfo.isLoaded model.musicInfo then
            div [ class "play" ]
                [ div [ class "play_header" ] []
                , div [ class "play_contentsContainer" ]
                    [ div [ class "play_contents" ]
                        [ Lanes.view model.lanes
                        , AllNotes.view model.currentMusicTime model.speed model.allNotes
                        , div [ class "play_display" ]
                            [ div [] [ text <| "currentMusicTime: " ++ String.fromFloat model.currentMusicTime ]
                            , div [] [ text <| "Bpm: " ++ MusicInfo.toStringBpm model.musicInfo ]
                            , div [] [ text <| "Score: " ++ Score.toString model.score ]
                            , div [] [ text <| "Combo: " ++ Combo.toString model.combo ]
                            , div [] [ text <| "MaxCombo: " ++ MusicInfo.toStringMaxCombo model.musicInfo ]
                            , div [] [ text "Spaceキーでスタート" ]
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



-- PORT


port getMusicInfo : () -> Cmd msg


port gotMusicInfo : (MusicInfoDto -> msg) -> Sub msg


port startMusic : () -> Cmd msg


port pauseMusic : () -> Cmd msg


port unPauseMusic : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playStatus of
        Playing ->
            Sub.batch
                [ Time.every 10 Tick
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]

        _ ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                , gotMusicInfo GotMusicInfo
                ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
