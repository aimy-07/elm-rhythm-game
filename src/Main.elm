port module Main exposing (main)

import AllMusicData exposing (AllMusicData)
import AudioManager.AudioLoadingS as AudioLoadingS exposing (AudioLoadingS)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Page
import Page.Blank as Blank
import Page.DataLoading as DataLoading
import Page.Error as Error
import Page.Home as Home
import Page.Init as Init
import Page.Play as Play
import Page.Title as Title
import Route exposing (Route)
import Session exposing (Session)
import Session.User exposing (UserDto)
import Url exposing (Url)



---- MODEL ----


{-|

    InitとDataLoadingは起動時のみ通過する。

    起動時の処理の流れ
    1. Init
    2. FirebaseのonAuthStateChangedの結果を受け取った(user == nullでもOK)ら、DataLoadingへ
    3. すべてのaudioと楽曲データを読み込み終わったらTitleへ
    4. 以降、TiTle, Home, Play, Errorをループする

-}
type Model
    = Init Session
    | DataLoading DataLoading.Model
    | Title Title.Model
    | Home Home.Model
    | Play Play.Model
    | Error Error.Model
    | Redirect Session AllMusicData AudioLoadingS


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    -- FirebaseのonAuthStateChangedのレスポンスを受け取るまでInit
    ( Init (Session.init navKey), Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
        Init session ->
            session

        DataLoading dataLoading ->
            DataLoading.toSession dataLoading

        Title title ->
            Title.toSession title

        Home home ->
            Home.toSession home

        Play play ->
            Play.toSession play

        Error error ->
            Error.toSession error

        Redirect session _ _ ->
            session


toAllMusicData : Model -> AllMusicData
toAllMusicData model =
    case model of
        Init _ ->
            AllMusicData.init

        DataLoading dataLoading ->
            DataLoading.toAllMusicData dataLoading

        Title title ->
            Title.toAllMusicData title

        Home home ->
            Home.toAllMusicData home

        Play play ->
            Play.toAllMusicData play

        Error error ->
            Error.toAllMusicData error

        Redirect _ allMusicData _ ->
            allMusicData


toAudioLoadingS : Model -> AudioLoadingS
toAudioLoadingS model =
    case model of
        Init _ ->
            AudioLoadingS.init

        DataLoading dataLoading ->
            DataLoading.toAudioLoadingS dataLoading

        Title title ->
            Title.toAudioLoadingS title

        Home home ->
            Home.toAudioLoadingS home

        Play play ->
            Play.toAudioLoadingS play

        Error error ->
            Error.toAudioLoadingS error

        Redirect _ _ audioLoadingS ->
            audioLoadingS



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotDataLoadingMsg DataLoading.Msg
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
                    ( model, Nav.pushUrl (Session.toNavKey <| toSession model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotDataLoadingMsg subMsg, DataLoading subModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    DataLoading.update subMsg subModel
                        |> updateWith DataLoading GotDataLoadingMsg model
            in
            if isDataLoaded updatedModel then
                -- 更新後のモデルに対して、初期データの読み込みが完了したかどうかをチェックする
                -- 読み込みが完了していたら、Titleへ遷移させる（以降、ModelがInit, DataLoadingに戻ることはない）
                Title.init (toSession updatedModel) (toAllMusicData updatedModel) (toAudioLoadingS updatedModel)
                    |> updateWith Title GotTitleMsg model

            else
                -- まだ読み込みが完了していなかったら、ローディング画面でロード処理を続行する
                ( updatedModel, updatedCmd )

        ( GotTitleMsg subMsg, Title subModel ) ->
            Title.update subMsg subModel
                |> updateWith Title GotTitleMsg model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg model

        ( GotPlayMsg subMsg, Play subModel ) ->
            Play.update subMsg subModel
                |> updateWith Play GotPlayMsg model

        ( GotErrorMsg subMsg, Error subModel ) ->
            Error.update subMsg subModel
                |> updateWith Error GotErrorMsg model

        ( ChangedAuth maybeUserDto, _ ) ->
            let
                navKey =
                    Session.toNavKey <| toSession model

                updatedSession =
                    Session.toLoggedIn navKey maybeUserDto

                ( updatedModel, cmd ) =
                    case model of
                        Init _ ->
                            -- ModelがInitの場合は、起動時のonAuthStateChangedにあたるのでDataLoadingへ遷移させる
                            DataLoading.init updatedSession
                                |> updateWith DataLoading GotDataLoadingMsg model

                        DataLoading dataLoading ->
                            -- DataLoadingの間にonAuthStateChangedが起きた場合は、セッションのみ更新し初期データの読み込みを続行する
                            -- 初期データの読み込みはauthと関係がなく、あとのタイトル画面でログイン処理を行うことになるので考慮する必要がない
                            ( DataLoading { dataLoading | session = updatedSession }, Cmd.none )

                        _ ->
                            -- それ以外の場合はタイトル画面に強制的に戻すので、Redirectに一度通す
                            ( Redirect updatedSession (toAllMusicData model) (toAudioLoadingS model), Cmd.none )

                nextRoute =
                    case model of
                        Init _ ->
                            Route.Title

                        DataLoading _ ->
                            Route.Title

                        _ ->
                            if Session.isLoggedIn <| updatedSession then
                                -- NotLogin -> LoggedIn or LoggedIn -> LoggedInであれば、userが存在するので直接Homeへ遷移させる
                                Route.Home

                            else
                                -- LoggedIn -> NotLogin or NotLogin -> NotLoginであれば、userが存在しないので強制的にTitleへ戻す
                                Route.Title
            in
            ( updatedModel, Cmd.batch [ cmd, Route.replaceUrl navKey nextRoute ] )

        ( DetectedError (), _ ) ->
            ( model, Route.replaceUrl (Session.toNavKey <| toSession model) Route.Error )

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

        allMusicData =
            toAllMusicData model

        audioLoadingS =
            toAudioLoadingS model

        maybeUserSetting =
            case model of
                Home home ->
                    Just (Home.toUserSetting home)

                _ ->
                    Nothing
    in
    case model of
        Init _ ->
            case maybeRoute of
                Just Route.Error ->
                    Error.init session allMusicData audioLoadingS
                        |> updateWith Error GotErrorMsg model

                _ ->
                    ( model, Cmd.none )

        DataLoading _ ->
            case maybeRoute of
                Just Route.Error ->
                    Error.init session allMusicData audioLoadingS
                        |> updateWith Error GotErrorMsg model

                _ ->
                    ( model, Cmd.none )

        _ ->
            case maybeRoute of
                Just Route.Title ->
                    Title.init session allMusicData audioLoadingS
                        |> updateWith Title GotTitleMsg model

                Just Route.Home ->
                    Home.init session allMusicData audioLoadingS
                        |> updateWith Home GotHomeMsg model

                Just (Route.Play maybeCsvFileName) ->
                    Play.init session allMusicData audioLoadingS maybeCsvFileName maybeUserSetting
                        |> updateWith Play GotPlayMsg model

                Just Route.Error ->
                    Error.init session allMusicData audioLoadingS
                        |> updateWith Error GotErrorMsg model

                Nothing ->
                    Title.init session allMusicData audioLoadingS
                        |> updateWith Title GotTitleMsg model


isDataLoaded : Model -> Bool
isDataLoaded model =
    AllMusicData.isLoaded (toAllMusicData model) && AudioLoadingS.isLoaded (toAudioLoadingS model)



-- PORT


port detectedError : (() -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subSubscriptions =
            case model of
                Init _ ->
                    Sub.none

                DataLoading dataLoading ->
                    Sub.map GotDataLoadingMsg (DataLoading.subscriptions dataLoading)

                Title title ->
                    Sub.map GotTitleMsg (Title.subscriptions title)

                Home home ->
                    Sub.map GotHomeMsg (Home.subscriptions home)

                Play play ->
                    Sub.map GotPlayMsg (Play.subscriptions play)

                Error error ->
                    Sub.map GotErrorMsg (Error.subscriptions error)

                Redirect _ _ _ ->
                    Sub.none
    in
    Sub.batch
        [ subSubscriptions
        , Session.onAuthStateChanged ChangedAuth
        , detectedError DetectedError
        ]



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage toMsg config =
            let
                { title, body } =
                    Page.view config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Init _ ->
            Page.view Init.view

        DataLoading dataLoading ->
            viewPage GotDataLoadingMsg (DataLoading.view dataLoading)

        Title title ->
            viewPage GotTitleMsg (Title.view title)

        Home home ->
            viewPage GotHomeMsg (Home.view home)

        Play play ->
            viewPage GotPlayMsg (Play.view play)

        Error error ->
            viewPage GotErrorMsg (Error.view error)

        Redirect _ _ _ ->
            Page.view Blank.view



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
