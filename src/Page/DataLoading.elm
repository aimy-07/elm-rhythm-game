module Page.DataLoading exposing
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
import AllMusicData.MusicData exposing (MusicDataCsvDto, MusicDataJsonDto)
import AudioManager
import AudioManager.AudioLoadingS as AudioLoadingS exposing (AudioLoadingS)
import AudioManager.BGM as BGM
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page
import Session exposing (Session)
import Utils exposing (cmdIf)



-- MODEL


type alias Model =
    { session : Session
    , allMusicData : AllMusicData
    , audioLoadingS : AudioLoadingS
    , viewState : ViewState
    }


type ViewState
    = Blank
    | Attention
    | DataLoading


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , allMusicData = AllMusicData.init
      , audioLoadingS = AudioLoadingS.init
      , viewState = Blank
      }
    , Cmd.batch
        [ AudioLoadingS.loadAudioInitial ()
        , AllMusicData.loadMusicDataByJsonCmds
        ]
    )


type Msg
    = LoadedAudioInitial ()
    | LoadedBGM String
    | LoadedSE String
    | LoadedMusicDataByJson MusicDataJsonDto
    | LoadedMusicDataByCsv MusicDataCsvDto
    | ClickedAudioAttention


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedAudioInitial _ ->
            let
                updatedAudioLoadingS =
                    AudioLoadingS.updateInitial model.audioLoadingS
            in
            ( { model | audioLoadingS = updatedAudioLoadingS, viewState = Attention }
            , Cmd.batch
                [ AudioLoadingS.loadBGM ()
                , AudioLoadingS.loadSE ()
                ]
            )

        LoadedBGM bgmKey ->
            let
                updatedAudioLoadingS =
                    model.audioLoadingS
                        |> AudioLoadingS.updateBGMLoadingProgress bgmKey
                        |> AudioLoadingS.update (model.viewState == DataLoading)
            in
            ( { model | audioLoadingS = updatedAudioLoadingS }, Cmd.none )

        LoadedSE seKey ->
            let
                updatedAudioLoadingS =
                    model.audioLoadingS
                        |> AudioLoadingS.updateSELoadingProgress seKey
                        |> AudioLoadingS.update (model.viewState == DataLoading)
            in
            ( { model | audioLoadingS = updatedAudioLoadingS }, Cmd.none )

        LoadedMusicDataByJson jsonDto ->
            let
                updatedAllMusicData =
                    AllMusicData.updateFromJson jsonDto model.allMusicData
            in
            ( { model | allMusicData = updatedAllMusicData }
            , AllMusicData.loadMusicDataByCsvCmds updatedAllMusicData
                |> cmdIf (AllMusicData.isLoadedJson updatedAllMusicData)
            )

        LoadedMusicDataByCsv csvDto ->
            let
                updatedAllMusicData =
                    AllMusicData.updateFromCsv csvDto model.allMusicData
            in
            ( { model | allMusicData = updatedAllMusicData }, Cmd.none )

        ClickedAudioAttention ->
            -- ブラウザの仕様で、ユーザー操作を一度介さないと音を再生することができない。
            -- 一度画面をクリックしたかどうかを判定し、クリック操作がないとAudioLoadingS == Loadedにならないようにする。
            let
                updatedAudioLoadingS =
                    model.audioLoadingS
                        |> AudioLoadingS.update True
            in
            ( { model | audioLoadingS = updatedAudioLoadingS, viewState = DataLoading }
            , AudioManager.playBGM BGM.TheRoadToHeaven Nothing
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AudioLoadingS.loadedAudioInitial LoadedAudioInitial
        , AudioLoadingS.loadedBGM LoadedBGM
        , AudioLoadingS.loadedSE LoadedSE
        , AllMusicData.loadedMusicDataByJson LoadedMusicDataByJson
        , AllMusicData.loadedMusicDataByCsv LoadedMusicDataByCsv
        ]



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Title"
    , content = div [ class "mainWide" ] [ viewContents model ]
    }


viewContents : Model -> Html Msg
viewContents model =
    let
        contents =
            case model.viewState of
                Blank ->
                    text ""

                Attention ->
                    viewAudioAttention

                DataLoading ->
                    viewDataLoading
    in
    div [ class "title_contentsContainer" ]
        [ contents
        , Page.viewLoaded
        ]


viewDataLoading : Html Msg
viewDataLoading =
    div
        [ class "title_contents" ]
        [ div
            [ class "titleBox_container" ]
            [ div [ class "titleBox_back" ] []
            , div [ class "titleBox_backInner" ] []
            , div [ class "titleBox_title" ] [ text "ELMusic" ]
            , div [ class "titleBox_subTitle" ] [ text "- Elmで開発されたリズムゲーム -" ]
            , div [ class "titleBox_loadingText" ] [ text "Loading" ]
            ]
        ]


viewAudioAttention : Html Msg
viewAudioAttention =
    div []
        [ div
            [ class "audioConfirm_contentsContainer" ]
            [ div [ class "audioConfirm_mainText" ] [ text "Welcome to ElMusic" ]
            , div [ class "audioConfirm_subText" ] [ text "- Please click anywhere on the screen -" ]
            , div
                [ class "audioConfirm_attentionContainer" ]
                [ div [ class "audioConfirm_attentionMainText" ] [ text "[注意] このゲームは音を使用します" ]
                , div [ class "audioConfirm_attentionSubText" ] [ text "・大きな音で耳を痛めないよう、音量にご注意ください。" ]
                , div [ class "audioConfirm_attentionSubText" ] [ text "・音を出せない環境でプレイする場合は、イヤホンやヘッドホンなどを用意して遊んでください。" ]
                , div [ class "audioConfirm_attentionSubText" ] [ text "・目や耳に疲れを感じたら、プレイを中断して休憩してください。" ]
                ]
            ]
        , div [ class "audioConfirm_screenBtn", onClick ClickedAudioAttention ] []
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
