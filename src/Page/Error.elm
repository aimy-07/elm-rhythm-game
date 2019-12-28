module Page.Error exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toAllMusicInfoList
    , toSession
    , update
    , view
    )

import AllMusicInfoList exposing (AllMusicInfoList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , allMusicInfoList : AllMusicInfoList
    }


init : Session -> AllMusicInfoList -> ( Model, Cmd Msg )
init session allMusicInfoList =
    ( { session = session
      , allMusicInfoList = allMusicInfoList
      }
    , Cmd.none
    )


type Msg
    = ClickedBackHomeButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedBackHomeButton ->
            let
                routeReplaceCmd =
                    if Session.isLoggedIn model.session then
                        Route.replaceUrl (Session.toNavKey model.session) Route.Home

                    else
                        Route.replaceUrl (Session.toNavKey model.session) Route.Login
            in
            ( model, routeReplaceCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, content : Html Msg }
view _ =
    { title = "Error"
    , content =
        div
            [ class "error_back" ]
            [ div
                [ class "error_contentsContainer" ]
                [ div
                    [ class "error_contents" ]
                    [ div [ class "error_largeText" ] [ text "- ERROR -" ]
                    , div [ class "error_smallText" ] [ text "通信エラーが発生しました" ]
                    , div
                        [ class "error_btnText", onClick ClickedBackHomeButton ]
                        [ text "- Back to Home -" ]
                    , a
                        [ class "error_btnText"
                        , href "https://github.com/aimy-07/elm-rhythm-game/issues"
                        , target "_blank"
                        ]
                        [ text "- Report -" ]
                    ]
                , div [ class "error_boxContainer" ] [ div [ class "error_box" ] [] ]
                ]
            ]
    }



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toAllMusicInfoList : Model -> AllMusicInfoList
toAllMusicInfoList model =
    model.allMusicInfoList
