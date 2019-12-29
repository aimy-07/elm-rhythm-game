port module Main exposing (main)

import AllMusicInfoList exposing (AllMusicInfoList)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import MusicInfo.CsvFileName exposing (CsvFileName)
import Page
import Page.Blank as Blank
import Page.Error as Error
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Play as Play
import Page.Title as Title
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import User exposing (UserDto)
import UserSetting exposing (UserSetting)



---- MODEL ----


type Model
    = Init Session UserSetting AllMusicInfoList
    | Redirect Session UserSetting AllMusicInfoList
    | NotFound Session UserSetting AllMusicInfoList
    | Title Title.Model
    | Home Home.Model
    | Play CsvFileName Play.Model
    | Error Error.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    -- onAuthChangedのレスポンスを受け取るまでInit
    ( Init (Session.init navKey) UserSetting.init AllMusicInfoList.init, Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
        Init session _ _ ->
            session

        Redirect session _ _ ->
            session

        NotFound session _ _ ->
            session

        Title title ->
            Title.toSession title

        Home home ->
            Home.toSession home

        Play _ play ->
            Play.toSession play

        Error error ->
            Error.toSession error


toUserSetting : Model -> UserSetting
toUserSetting model =
    case model of
        Init _ userSetting _ ->
            userSetting

        Redirect _ userSetting _ ->
            userSetting

        NotFound _ userSetting _ ->
            userSetting

        Title title ->
            Title.toUserSetting title

        Home home ->
            Home.toUserSetting home

        Play _ play ->
            Play.toUserSetting play

        Error error ->
            Error.toUserSetting error


toAllMusicInfoList : Model -> AllMusicInfoList
toAllMusicInfoList model =
    case model of
        Init _ _ allMusicInfoList ->
            allMusicInfoList

        Redirect _ _ allMusicInfoList ->
            allMusicInfoList

        NotFound _ _ allMusicInfoList ->
            allMusicInfoList

        Title title ->
            Title.toAllMusicInfoList title

        Home home ->
            Home.toAllMusicInfoList home

        Play _ play ->
            Play.toAllMusicInfoList play

        Error error ->
            Error.toAllMusicInfoList error



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotTitleMsg Title.Msg
    | GotHomeMsg Home.Msg
    | GotPlayMsg Play.Msg
    | GotErrorMsg Error.Msg
    | ChangedAuth (Maybe UserDto)
    | DetectedError ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        navKey =
                            Session.toNavKey (toSession model)
                    in
                    ( model, Nav.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotTitleMsg subMsg, Title subModel ) ->
            Title.update subMsg subModel
                |> updateWith Title GotTitleMsg model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg model

        ( GotPlayMsg subMsg, Play csvFileName subModel ) ->
            Play.update subMsg subModel
                |> updateWith (Play csvFileName) GotPlayMsg model

        ( GotErrorMsg subMsg, Error subModel ) ->
            Error.update subMsg subModel
                |> updateWith Error GotErrorMsg model

        ( ChangedAuth maybeUserDto, _ ) ->
            let
                navKey =
                    Session.toNavKey <| toSession model

                updatedSession =
                    Session.toLoggedIn navKey maybeUserDto

                nextRoute =
                    case model of
                        Init _ _ _ ->
                            Route.Title

                        _ ->
                            if Session.isLoggedIn updatedSession then
                                Route.Home

                            else
                                Route.Title
            in
            ( Redirect updatedSession (toUserSetting model) (toAllMusicInfoList model)
            , Route.replaceUrl navKey nextRoute
            )

        ( DetectedError (), _ ) ->
            let
                navKey =
                    Session.toNavKey <| toSession model
            in
            ( model, Route.replaceUrl navKey Route.Error )

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model

        allMusicInfoList =
            toAllMusicInfoList model

        userSetting =
            toUserSetting model
    in
    case model of
        Init _ _ _ ->
            ( model, Cmd.none )

        _ ->
            case maybeRoute of
                Nothing ->
                    ( NotFound session userSetting allMusicInfoList, Cmd.none )

                Just Route.Title ->
                    Title.init session userSetting allMusicInfoList
                        |> updateWith Title GotTitleMsg model

                Just Route.Home ->
                    Home.init session userSetting allMusicInfoList
                        |> updateWith Home GotHomeMsg model

                Just (Route.Play maybeCsvFileName) ->
                    case maybeCsvFileName of
                        Just csvFileName ->
                            Play.init session userSetting allMusicInfoList csvFileName
                                |> updateWith (Play csvFileName) GotPlayMsg model

                        Nothing ->
                            Home.init session userSetting allMusicInfoList
                                |> updateWith Home GotHomeMsg model

                Just Route.Error ->
                    Error.init session userSetting allMusicInfoList
                        |> updateWith Error GotErrorMsg model



-- PORT


port onAuthStateChanged : (Maybe UserDto -> msg) -> Sub msg


port detectedError : (() -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subSubscriptions =
            case model of
                Init _ _ _ ->
                    Sub.none

                Redirect _ _ _ ->
                    Sub.none

                NotFound _ _ _ ->
                    Sub.none

                Title title ->
                    Sub.map GotTitleMsg (Title.subscriptions title)

                Home home ->
                    Sub.map GotHomeMsg (Home.subscriptions home)

                Play _ play ->
                    Sub.map GotPlayMsg (Play.subscriptions play)

                Error error ->
                    Sub.map GotErrorMsg (Error.subscriptions error)
    in
    Sub.batch
        [ subSubscriptions
        , onAuthStateChanged ChangedAuth
        , detectedError DetectedError
        ]



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
        Init _ _ _ ->
            Page.view Page.Other Blank.view

        Redirect _ _ _ ->
            Page.view Page.Other Blank.view

        NotFound _ _ _ ->
            Page.view Page.Other NotFound.view

        Title title ->
            viewPage Page.Title GotTitleMsg (Title.view title)

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Play _ play ->
            viewPage Page.Play GotPlayMsg (Play.view play)

        Error error ->
            viewPage Page.Error GotErrorMsg (Error.view error)



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
