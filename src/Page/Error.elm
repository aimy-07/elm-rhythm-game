module Page.Error exposing
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
import AudioManager
import AudioManager.AudioLoadingS exposing (AudioLoadingS)
import AudioManager.SE as SE
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick, onMouseUp)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , allMusicData : AllMusicData
    , audioLoadingS : AudioLoadingS
    }


init : Session -> AllMusicData -> AudioLoadingS -> ( Model, Cmd Msg )
init session audioMusicData audioLoadingS =
    ( { session = session
      , allMusicData = audioMusicData
      , audioLoadingS = audioLoadingS
      }
    , Cmd.batch
        [ AudioManager.stopBGM ()
        , AudioManager.playSE SE.Attention Nothing
        ]
    )


type Msg
    = ClickedBackTitleButton
    | PlayReportBtnSE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedBackTitleButton ->
            ( model
            , Cmd.batch
                [ Route.replaceUrl (Session.toNavKey model.session) Route.Title
                , AudioManager.playSE SE.Cancel Nothing
                ]
            )

        PlayReportBtnSE ->
            ( model, AudioManager.playSE SE.Select Nothing )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view _ =
    div
        [ class "error_back" ]
        [ div
            [ class "error_overview" ]
            [ div
                [ class "error_textContainer" ]
                [ div [ class "error_largeText" ] [ text "- ERROR -" ]
                , div [ class "error_smallText" ] [ text "通信エラーが発生しました" ]
                , div
                    [ class "error_btnText", onClick ClickedBackTitleButton ]
                    [ text "- Back to Title -" ]
                , a
                    [ class "error_btnText"
                    , href "https://github.com/aimy-07/elm-rhythm-game/issues"
                    , target "_blank"
                    , onMouseUp PlayReportBtnSE
                    ]
                    [ text "- Report -" ]
                ]
            , div [ class "error_boxContainer" ] [ div [ class "error_box" ] [] ]
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
