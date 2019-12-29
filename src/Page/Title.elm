port module Page.Title exposing
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
import MusicInfo exposing (MusicInfoDto)
import MusicInfo.CsvFileName as CsvFileName
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
        [ getAllMusicInfoList ()
            |> cmdIf (not <| AllMusicInfoList.isLoaded allMusicInfoList)
        , playTitleBgm ()
        ]
    )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)
    | GotAllSampleAudio ()
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
                    AllMusicInfoList.new musicInfoDtos

                audioFileNameList =
                    musicInfoDtos
                        |> List.map (.csvFileName >> CsvFileName.toAudioFileName)
            in
            ( { model | allMusicInfoList = allMusicInfoList }
            , getAllSampleAudio audioFileNameList
            )

        GotAllSampleAudio _ ->
            let
                allMusicInfoList =
                    AllMusicInfoList.ready model.allMusicInfoList
            in
            ( { model | allMusicInfoList = allMusicInfoList }
            , Cmd.none
            )

        ClickedStartBtn ->
            ( { model | loginBtnS = LoginBtnS.toShow model.loginBtnS }, Cmd.none )

        ClickedGoogleLoginBtn ->
            ( { model | loginBtnS = LoginBtnS.toDisabled Google model.loginBtnS }
            , signInWithGoogle ()
            )

        ClickedTwitterLoginBtn ->
            ( { model | loginBtnS = LoginBtnS.toDisabled Twitter model.loginBtnS }
            , signInWithTwitter ()
            )

        ClickedGithubLoginBtn ->
            ( { model | loginBtnS = LoginBtnS.toDisabled Github model.loginBtnS }
            , signInWithGithub ()
            )

        CanceledSignIn _ ->
            ( { model | loginBtnS = LoginBtnS.toShow model.loginBtnS }, Cmd.none )



-- PORT


port getAllMusicInfoList : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg


port getAllSampleAudio : List String -> Cmd msg


port gotAllSampleAudio : (() -> msg) -> Sub msg


port signInWithGoogle : () -> Cmd msg


port signInWithTwitter : () -> Cmd msg


port signInWithGithub : () -> Cmd msg


port canceledSignIn : (() -> msg) -> Sub msg


port playTitleBgm : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotAllMusicInfoList GotAllMusicInfoList
        , gotAllSampleAudio GotAllSampleAudio
        , canceledSignIn CanceledSignIn
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
