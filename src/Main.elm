module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import MusicInfo.CsvFileName exposing (CsvFileName)
import Page exposing (Page)
import Page.Blank as Blank
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Play as Play
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



---- MODEL ----


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Play CsvFileName Play.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey))



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
        Redirect _ ->
            Page.view Page.Other Blank.view

        NotFound _ ->
            Page.view Page.Other NotFound.view

        Home subModel ->
            viewPage Page.Home GotHomeMsg (Home.view subModel)

        Play csvFileName subModel ->
            viewPage Page.Play GotPlayMsg (Play.view subModel)



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotPlayMsg Play.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home subModel ->
            Home.toSession subModel

        Play _ subModel ->
            Play.toSession subModel


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just (Route.Play csvFileName) ->
            Play.init session csvFileName
                |> updateWith (Play csvFileName) GotPlayMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
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

        ( GotPlayMsg subMsg, Play csvFileName subModel ) ->
            Play.update subMsg subModel
                |> updateWith (Play csvFileName) GotPlayMsg model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Sub.none

        Home subModel ->
            Sub.map GotHomeMsg (Home.subscriptions subModel)

        Play _ subModel ->
            Sub.map GotPlayMsg (Play.subscriptions subModel)



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
