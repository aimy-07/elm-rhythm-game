port module Page.Play exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Page
import Page.Play.Combo as Combo exposing (Combo)
import Page.Play.ConcurrentNotes as ConcurrentNotes exposing (ConcurrentNotes)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.JudgeEffect as JudgeEffect exposing (JudgeEffect)
import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.JustTime exposing (JustTime)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)
import Page.Play.LongNoteLine as LongNoteLine exposing (EndTime, LongNoteLine)
import Page.Play.MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import Page.Play.Note as Note exposing (Note)
import Page.Play.Score as Score exposing (Score)
import Page.Play.Speed exposing (Speed)
import Route
import Session exposing (Session)
import Task
import Time



-- MODEL


type alias Model =
    { session : Session
    , musicInfo : MusicInfo
    , playStatus : PlayStatus
    , currentMusicTime : CurrentMusicTime
    , pressingLines : List LinePosition
    , longPressingLines : List LongNoteLine
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
      , musicInfo = MusicInfo.init
      , playStatus = NotStart
      , currentMusicTime = 0
      , pressingLines = []
      , longPressingLines = []
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
                    model.currentMusicTime + 10

                isOverMiss =
                    List.head (MusicInfo.toAllNotes model.musicInfo)
                        |> Maybe.map (\head -> ConcurrentNotes.toJustTime head)
                        |> JudgeKind.isOverMiss updatedTime

                nextMusicInfo =
                    model.musicInfo
                        |> Page.updateIf isOverMiss MusicInfo.updateNotesOverMiss

                nextLongPressingLines =
                    model.longPressingLines
                        |> List.filter
                            (\line -> LongNoteLine.toEndTime line > updatedTime)

                nextScore =
                    model.score
                        |> Score.addLong (List.length nextLongPressingLines)

                nextCombo =
                    model.combo
                        |> Page.updateIf isOverMiss (Combo.update JudgeKind.miss)

                missEffectCmd =
                    List.head (MusicInfo.toAllNotes model.musicInfo)
                        |> Maybe.map
                            (\head ->
                                ConcurrentNotes.toNotes head
                                    |> List.map
                                        (\note ->
                                            JudgeEffect.new JudgeKind.miss (Note.toPosition note)
                                                |> addJudgeEffect
                                        )
                                    |> Cmd.batch
                            )
                        |> Maybe.withDefault Cmd.none
                        |> Page.cmdIf isOverMiss

                nextPlayStatus =
                    model.playStatus
                        |> Page.updateIf (MusicInfo.toFullTime model.musicInfo < model.currentMusicTime) (always Finish)
            in
            ( { model
                | currentMusicTime = updatedTime
                , playStatus = nextPlayStatus
                , musicInfo = nextMusicInfo
                , longPressingLines = nextLongPressingLines
                , score = nextScore
                , combo = nextCombo
              }
            , missEffectCmd
            )

        GotMusicInfo rawMusicInfo ->
            let
                nextMusicInfo =
                    MusicInfo.create rawMusicInfo

                _ =
                    Debug.log "rawMusicInfo" rawMusicInfo
            in
            ( { model | musicInfo = nextMusicInfo }, Cmd.none )

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
                                    ( Playing, startMusic () )

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
                        position =
                            LinePosition.new keyStr
                    in
                    if List.member position model.pressingLines then
                        ( model, Cmd.none )

                    else
                        let
                            judgeKind =
                                List.head (MusicInfo.toAllNotes model.musicInfo)
                                    |> JudgeKind.judgeKeyDown model.currentMusicTime position

                            nextMusicInfo =
                                model.musicInfo
                                    |> Page.updateIf (not <| JudgeKind.isInvalid judgeKind) (MusicInfo.updateNotesKeyDown position)

                            endTime =
                                List.head (MusicInfo.toAllNotes model.musicInfo)
                                    |> LongNoteLine.getEndTime model.currentMusicTime position

                            nextLongPressingLines =
                                model.longPressingLines
                                    |> Page.updateIf (not <| JudgeKind.isInvalid judgeKind) ((::) (LongNoteLine.new position endTime))

                            nextPressingLines =
                                model.pressingLines
                                    |> Page.updateIf (not <| List.member position model.pressingLines) ((::) position)

                            judgeEffectCmd =
                                JudgeEffect.new judgeKind position
                                    |> addJudgeEffect
                                    |> Page.cmdIf (not <| JudgeKind.isInvalid judgeKind)
                        in
                        ( { model
                            | musicInfo = nextMusicInfo
                            , longPressingLines = nextLongPressingLines
                            , pressingLines = nextPressingLines
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
                        position =
                            LinePosition.new keyStr

                        nextLongPressingLines =
                            model.longPressingLines
                                |> List.filter (\line -> LongNoteLine.toPosition line /= position)

                        nextPressingLines =
                            model.pressingLines
                                |> List.filter (\line -> line /= position)
                    in
                    ( { model
                        | pressingLines = nextPressingLines
                        , longPressingLines = nextLongPressingLines
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
                        [ viewLines model
                        , div [ class "play_centerLine", id "judge_area" ]
                            [ viewLongNoteLines model
                            , viewConcurrentNotes model
                            ]
                        ]
                    , div [ class "play_display" ]
                        [ div [] [ text <| "currentMusicTime: " ++ String.fromFloat model.currentMusicTime ]
                        , div [] [ text <| "Bpm: " ++ MusicInfo.toStringBpm model.musicInfo ]
                        , div [] [ text <| "Score: " ++ Score.toString model.score ]
                        , div [] [ text <| "Combo: " ++ Combo.toString model.combo ]
                        , div [] [ text "Spaceキーでスタート" ]
                            |> Page.viewIf (model.playStatus == NotStart)
                        , div [] [ text "Finish!" ]
                            |> Page.viewIf (model.playStatus == Finish)
                        , a [ Route.href Route.Home ] [ text "Homeに戻る" ]
                            |> Page.viewIf (model.playStatus == Finish)
                        ]
                    ]
                ]

        else
            text ""
    }


viewConcurrentNotes : Model -> Html msg
viewConcurrentNotes model =
    div []
        (MusicInfo.toAllNotes model.musicInfo
            |> List.map
                (\notes ->
                    ConcurrentNotes.view model.currentMusicTime model.speed notes
                )
        )


viewLongNoteLines : Model -> Html msg
viewLongNoteLines model =
    div []
        (model.longPressingLines
            |> List.map
                (\line ->
                    LongNoteLine.view model.currentMusicTime model.speed line
                )
        )


viewLines : Model -> Html msg
viewLines model =
    div []
        (LinePosition.allLines
            |> List.map
                (\line ->
                    LinePosition.viewLine line model.pressingLines
                )
        )



-- PORT


port getMusicInfo : () -> Cmd msg


port gotMusicInfo : (MusicInfoDto -> msg) -> Sub msg


port addJudgeEffect : JudgeEffect -> Cmd msg


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
