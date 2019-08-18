module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )



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
