module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Home.MusicInfo as MusicInfo exposing (MusicInfo)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , normalMusics : List MusicInfo
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , normalMusics = MusicInfo.normalMusics
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div []
            [ h1 [] [ text "ホーム画面" ]
            , div []
                (List.map (\musicInfo -> MusicInfo.view musicInfo) model.normalMusics)
            , a [ Route.href Route.Play ] [ text "プレイ画面へ" ]
            ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
