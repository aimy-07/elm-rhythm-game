module Page.Title exposing
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
import AudioManager
import AudioManager.AudioInfo exposing (AudioInfoDto)
import Constants exposing (currentModeDefault)
import Html exposing (Html, a, br, button, div, img, li, text)
import Html.Attributes exposing (class, disabled, href, src)
import Html.Events exposing (onClick)
import MusicInfo exposing (MusicInfoDto)
import MusicInfo.CsvFileName as CsvFileName
import MusicInfo.Mode as Mode
import Page.Title.LoginBtnS as LoginBtnS exposing (LoginBtnS)
import Route
import Session exposing (Session)
import User.AccountType as AccountType exposing (AccountType(..))
import Utils exposing (cmdIf, viewIf)



-- MODEL


type alias Model =
    { session : Session
    , allMusicInfoList : AllMusicInfoList
    , loginBtnS : LoginBtnS
    }


init : Session -> AllMusicInfoList -> ( Model, Cmd Msg )
init session allMusicInfoList =
    ( { session = session
      , allMusicInfoList = allMusicInfoList
      , loginBtnS = LoginBtnS.init
      }
    , Cmd.batch
        [ AllMusicInfoList.getAllMusicInfoList allMusicInfoList
        , AudioManager.playTitleBgm ()
        ]
    )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)
    | GotAudioInfo AudioInfoDto
    | ClickedStartBtn
    | ClickedGoogleLoginBtn
    | ClickedTwitterLoginBtn
    | ClickedGithubLoginBtn
    | CanceledSignIn ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllMusicInfoList musicInfoDtos ->
            let
                allMusicInfoList =
                    AllMusicInfoList.setMusicInfo musicInfoDtos model.allMusicInfoList

                audioFileNames =
                    musicInfoDtos
                        |> List.filter
                            (\musicInfo ->
                                Mode.new musicInfo.mode == currentModeDefault
                            )
                        |> List.map
                            (\musicInfo ->
                                CsvFileName.toMusicId musicInfo.csvFileName ++ "_sample"
                            )
            in
            ( { model | allMusicInfoList = allMusicInfoList }
            , Cmd.batch <| List.map AudioManager.getAudioInfo audioFileNames
            )

        GotAudioInfo audioInfoDto ->
            let
                allMusicInfoList =
                    AllMusicInfoList.setAudioInfo audioInfoDto model.allMusicInfoList
            in
            ( { model | allMusicInfoList = allMusicInfoList }, Cmd.none )

        ClickedStartBtn ->
            ( { model | loginBtnS = LoginBtnS.toShow model.loginBtnS }, Cmd.none )

        ClickedGoogleLoginBtn ->
            ( { model | loginBtnS = LoginBtnS.toDisabled Google model.loginBtnS }
            , Session.signInWithGoogle ()
            )

        ClickedTwitterLoginBtn ->
            ( { model | loginBtnS = LoginBtnS.toDisabled Twitter model.loginBtnS }
            , Session.signInWithTwitter ()
            )

        ClickedGithubLoginBtn ->
            ( { model | loginBtnS = LoginBtnS.toDisabled Github model.loginBtnS }
            , Session.signInWithGithub ()
            )

        CanceledSignIn _ ->
            ( { model | loginBtnS = LoginBtnS.toShow model.loginBtnS }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AllMusicInfoList.gotAllMusicInfoList GotAllMusicInfoList
        , AudioManager.gotAudioInfo GotAudioInfo
        , Session.canceledSignIn CanceledSignIn
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
        isLoaded =
            AllMusicInfoList.isLoaded model.allMusicInfoList

        isLoggedIn =
            Session.isLoggedIn model.session
    in
    div [ class "title_contentsContainer" ]
        [ div
            [ class "titleCubeEffects" ]
            (List.map (\_ -> li [] []) (List.range 0 16))
        , div
            [ class "title_contents" ]
            [ div
                [ class "titleBox_container" ]
                [ div [ class "titleBox_back" ] []
                , div [ class "titleBox_backInner" ] []
                , div [ class "titleBox_title" ] [ text "ELMusic" ]
                , div [ class "titleBox_subTitle" ] [ text "- Elmで開発されたリズムゲーム -" ]
                , a [ class "titleBox_startBtn", Route.href Route.Home ] [ text "START" ]
                    |> viewIf (isLoaded && isLoggedIn)
                , button [ class "titleBox_startBtn is-disabled", onClick ClickedStartBtn ] [ text "START" ]
                    |> viewIf (isLoaded && not isLoggedIn)
                , div [ class "titleBox_loadingText" ] [ text "Loading" ]
                    |> viewIf (not isLoaded)
                ]
            ]
        , viewOverView model.loginBtnS
            |> viewIf (LoginBtnS.isShow model.loginBtnS)
        ]


viewOverView : LoginBtnS -> Html Msg
viewOverView loginBtnS =
    div [ class "title_overview" ]
        [ div
            [ class "titleOverview_container" ]
            [ div
                [ class "titleOverview_LoginBoxesContainer" ]
                [ viewLoginBtn Google loginBtnS
                , viewLoginBtn Twitter loginBtnS
                , viewLoginBtn Github loginBtnS
                ]
            ]
        ]


viewLoginBtn : AccountType -> LoginBtnS -> Html Msg
viewLoginBtn accountType loginBtnS =
    let
        ( msg, colorStr, imgSrc ) =
            case accountType of
                Google ->
                    ( ClickedGoogleLoginBtn, "orange", "./img/icon_google.png" )

                Twitter ->
                    ( ClickedTwitterLoginBtn, "lightblue", "./img/icon_twitter.png" )

                Github ->
                    ( ClickedGithubLoginBtn, "green", "./img/icon_github.png" )

        clsBtnStyle =
            if LoginBtnS.isDisabled loginBtnS then
                if LoginBtnS.isSelecting accountType loginBtnS then
                    "is-selecting"

                else
                    "is-disabled"

            else
                ""
    in
    div
        [ class "titleOverviewLoginBox_container"
        , class clsBtnStyle
        ]
        [ button
            [ class "titleOverviewLoginBox_back"
            , onClick msg
            , disabled <| LoginBtnS.isDisabled loginBtnS
            ]
            []
        , div [ class "titleOverviewLoginBox_backInner", class colorStr ] []
        , img [ class "titleOverviewLoginBox_icon", src imgSrc ] []
        , div
            [ class "titleOverviewLoginBox_text" ]
            [ text <| AccountType.toString accountType ++ "アカウントで"
            , br [] []
            , text "ログイン"
            ]
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toAllMusicInfoList : Model -> AllMusicInfoList
toAllMusicInfoList model =
    model.allMusicInfoList
