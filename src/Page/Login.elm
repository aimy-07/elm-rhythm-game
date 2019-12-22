port module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedGoogleLoginBtn
    | ClickedTwitterLoginBtn
    | ClickedGithubLoginBtn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGoogleLoginBtn ->
            ( model, signInWithGoogle () )

        ClickedTwitterLoginBtn ->
            ( model, signInWithTwitter () )

        ClickedGithubLoginBtn ->
            ( model, signInWithGithub () )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content = div [ class "mainWide" ] [ viewContents model ]
    }


viewContents : Model -> Html Msg
viewContents model =
    div []
        [ h1 [] [ text "ログインページ" ]
        , button [ onClick ClickedGoogleLoginBtn ] [ text "Googleアカウントでログイン" ]
        , button [ onClick ClickedTwitterLoginBtn ] [ text "Twitterアカウントでログイン" ]
        , button [ onClick ClickedGithubLoginBtn ] [ text "Githubアカウントでログイン" ]
        ]



-- PORT


port signInWithGoogle : () -> Cmd msg


port signInWithTwitter : () -> Cmd msg


port signInWithGithub : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
