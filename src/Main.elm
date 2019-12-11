port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import MusicInfo.CsvFileName exposing (CsvFileName)
import Page
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Play as Play
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import User exposing (UserDto)



---- MODEL ----


type Model
    = Init Session
    | Redirect Session
    | NotFound Session
    | Home Home.Model
    | Login Login.Model
    | Play CsvFileName Play.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    -- onAuthChangedのレスポンスを受け取るまでInit
    ( Init (Session.init navKey), Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Init _ ->
            Page.view Page.Other Blank.view

        Redirect _ ->
            Page.view Page.Other Blank.view

        NotFound _ ->
            Page.view Page.Other NotFound.view

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Login login ->
            viewPage Page.Other GotLoginMsg (Login.view login)

        Play _ play ->
            viewPage Page.Play GotPlayMsg (Play.view play)



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotPlayMsg Play.Msg
    | ChangedAuth (Maybe UserDto)


toSession : Model -> Session
toSession model =
    case model of
        Init session ->
            session

        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        Play _ play ->
            Play.toSession play


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model

        maybeUser =
            Session.toUser session
    in
    case model of
        Init _ ->
            ( model, Cmd.none )

        _ ->
            case maybeRoute of
                Nothing ->
                    ( NotFound session, Cmd.none )

                Just Route.Home ->
                    Home.init session
                        |> updateWith Home GotHomeMsg model

                Just Route.Login ->
                    Login.init session
                        |> updateWith Login GotLoginMsg model

                Just (Route.Play csvFileName) ->
                    Play.init session csvFileName
                        |> updateWith (Play csvFileName) GotPlayMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.toNavKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg model

        ( GotLoginMsg subMsg, Login subModel ) ->
            Login.update subMsg subModel
                |> updateWith Login GotLoginMsg model

        ( GotPlayMsg subMsg, Play csvFileName subModel ) ->
            Play.update subMsg subModel
                |> updateWith (Play csvFileName) GotPlayMsg model

        ( ChangedAuth maybeUserDto, _ ) ->
            let
                session =
                    toSession model

                updatedSession =
                    Session.fromUser (Session.toNavKey session) maybeUserDto

                replaceUrlCmd =
                    maybeUserDto
                        |> Maybe.map
                            (\_ -> Route.replaceUrl (Session.toNavKey session) Route.Home)
                        |> Maybe.withDefault
                            (Route.replaceUrl (Session.toNavKey session) Route.Login)
            in
            ( Redirect updatedSession
            , replaceUrlCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- PORT


port onAuthStateChanged : (Maybe UserDto -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subSubscriptions =
            case model of
                Init _ ->
                    Sub.none

                Redirect _ ->
                    Sub.none

                NotFound _ ->
                    Sub.none

                Home home ->
                    Sub.map GotHomeMsg (Home.subscriptions home)

                Login login ->
                    Sub.map GotLoginMsg (Login.subscriptions login)

                Play _ play ->
                    Sub.map GotPlayMsg (Play.subscriptions play)
    in
    Sub.batch
        [ subSubscriptions
        , onAuthStateChanged ChangedAuth
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
