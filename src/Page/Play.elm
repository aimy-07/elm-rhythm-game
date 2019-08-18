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
import Page.Play.MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import Page.Play.Note as Note exposing (EndTime, Note)
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
    , longPressingLines : List { position : LinePosition, endTime : EndTime }
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

                isMiss =
                    List.head (MusicInfo.toAllNotes model.musicInfo)
                        |> Maybe.map (\head -> ConcurrentNotes.toJustTime head)
                        |> JudgeKind.isMiss updatedTime

                nextMusicInfo =
                    if isMiss then
                        MusicInfo.updateNotesOverMiss model.musicInfo

                    else
                        model.musicInfo

                missEffectCmd =
                    if isMiss then
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

                    else
                        Cmd.none

                nextCombo =
                    if isMiss then
                        Combo.update JudgeKind.miss model.combo

                    else
                        model.combo

                nextLongPressingLines =
                    model.longPressingLines
                        |> List.filter
                            (\line -> line.endTime > updatedTime)

                nextScore =
                    Score.addLong (List.length nextLongPressingLines) model.score

                nextPlayStatus =
                    if MusicInfo.toFullTime model.musicInfo < updatedTime then
                        Finish

                    else
                        model.playStatus
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

                        judgeKind =
                            List.head (MusicInfo.toAllNotes model.musicInfo)
                                |> JudgeKind.judgeKeyDown model.currentMusicTime position

                        nextMusicInfo =
                            if JudgeKind.isInvalid judgeKind then
                                model.musicInfo

                            else
                                MusicInfo.updateNotesKeyDown position model.musicInfo

                        judgeEffectCmd =
                            if JudgeKind.isInvalid judgeKind then
                                Cmd.none

                            else
                                JudgeEffect.new judgeKind position
                                    |> addJudgeEffect

                        nextLongPressingLines =
                            if JudgeKind.isInvalid judgeKind then
                                model.longPressingLines

                            else
                                let
                                    endTime =
                                        List.head (MusicInfo.toAllNotes model.musicInfo)
                                            |> Maybe.map
                                                (\head ->
                                                    let
                                                        justTime =
                                                            ConcurrentNotes.toJustTime head

                                                        notes =
                                                            ConcurrentNotes.toNotes head
                                                    in
                                                    notes
                                                        |> List.filter (\note -> Note.toPosition note == position)
                                                        |> List.head
                                                        |> Maybe.map (\note -> justTime + Note.toLongTime note)
                                                        |> Maybe.withDefault 0
                                                )
                                            |> Maybe.withDefault 0
                                in
                                { position = position, endTime = endTime } :: model.longPressingLines

                        nextPressingLines =
                            if List.member position model.pressingLines then
                                model.pressingLines

                            else
                                position :: model.pressingLines
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
                                |> List.filter (\line -> line.position /= position)

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
            div []
                [ div
                    [ style "position" "relative"
                    , style "height" "620px"
                    ]
                    [ div
                        [ style "position" "absolute"
                        , style "top" "500px"
                        , style "left" "0px"
                        , style "width" "1000px"
                        , style "height" "10px"
                        , style "background-color" "gray"
                        , id "judge_area"
                        ]
                        (List.map (\line -> viewLine line model) LinePosition.allLines
                            ++ List.map (\notes -> viewConcurrentNotes notes model) (MusicInfo.toAllNotes model.musicInfo)
                            ++ [ viewLongNotes model.longPressingLines model ]
                        )
                    ]
                , div [] [ text <| "currentMusicTime: " ++ String.fromFloat model.currentMusicTime ]
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

        else
            text ""
    }


viewConcurrentNotes : ConcurrentNotes -> Model -> Html msg
viewConcurrentNotes concurrentNotes model =
    let
        justTime =
            ConcurrentNotes.toJustTime concurrentNotes
    in
    div []
        (ConcurrentNotes.toNotes concurrentNotes
            |> List.map (\note -> Note.view note model.currentMusicTime justTime model.speed)
        )


viewLongNotes : List { position : LinePosition, endTime : EndTime } -> Model -> Html msg
viewLongNotes longPressingLines model =
    div []
        (longPressingLines
            |> List.map
                (\line ->
                    let
                        height =
                            (line.endTime - model.currentMusicTime) * model.speed
                    in
                    div
                        [ class "note"
                        , class "long"
                        , style "height" (String.fromFloat height ++ "px")
                        , style "bottom" "10px"
                        , style "left" (LinePosition.styleLeft <| line.position)
                        ]
                        []
                )
        )


viewLine : LinePosition -> Model -> Html msg
viewLine linePosition model =
    let
        isPressing =
            List.member linePosition model.pressingLines
    in
    div
        [ class "line"
        , class <|
            if isPressing then
                " is-pressing"

            else
                ""
        , style "left" (LinePosition.styleLeft linePosition)
        ]
        []



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
