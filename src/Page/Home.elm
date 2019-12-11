port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.Mode as Mode exposing (Mode)
import Page
import Page.Home.AllMusicInfoList as AllMusicInfoList exposing (AllMusicInfoList)
import Rank exposing (Rank)
import Route
import Session exposing (Session)
import User



-- MODEL


type alias Model =
    { session : Session
    , allMusicInfoList : AllMusicInfoList
    , maybeCurrentMusicInfo : Maybe MusicInfo
    , maybeCurrentMode : Maybe Mode
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , allMusicInfoList = AllMusicInfoList.init
      , maybeCurrentMusicInfo = Nothing
      , maybeCurrentMode = Nothing
      }
    , Cmd.batch
        [ getAllMusicInfoList ()
        , startHomeMusic ()
        ]
    )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)
    | ChangeMode Mode
    | ChangeMusic MusicInfo
    | SignOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllMusicInfoList musicInfoDtos ->
            let
                allMusicInfoList =
                    musicInfoDtos
                        |> List.map MusicInfo.new
                        |> AllMusicInfoList.create

                -- TODO: 前回選択したMode、曲名を取ってきた時にやる
                currentMusicInfo =
                    allMusicInfoList
                        |> AllMusicInfoList.filteredMusicInfoListByMode Mode.normal
                        |> List.head
            in
            ( { model
                | allMusicInfoList = allMusicInfoList
                , maybeCurrentMusicInfo = currentMusicInfo
                , maybeCurrentMode = Just Mode.normal
              }
              -- TODO: 前回選択したMode、曲名を取ってくる
            , Cmd.none
            )

        ChangeMode mode ->
            case ( model.maybeCurrentMode, model.maybeCurrentMusicInfo ) of
                ( Just currentMode, Just currentMusicInfo ) ->
                    let
                        nextCurrentMusicInfo =
                            model.allMusicInfoList
                                |> AllMusicInfoList.filteredMusicInfoListByMode mode
                                |> List.filter
                                    (\musicInfo ->
                                        MusicInfo.toMusicName musicInfo == MusicInfo.toMusicName currentMusicInfo
                                    )
                                |> List.head
                    in
                    ( { model
                        | maybeCurrentMode = Just mode
                        , maybeCurrentMusicInfo = nextCurrentMusicInfo
                      }
                    , playMusicSelectAnim ()
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeMusic musicInfo ->
            ( { model | maybeCurrentMusicInfo = Just musicInfo }
            , playMusicSelectAnim ()
            )

        SignOut ->
            ( model, signOut () )



-- PORT


port getAllMusicInfoList : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg


port playMusicSelectAnim : () -> Cmd msg


port startHomeMusic : () -> Cmd msg


port signOut : () -> Cmd msg



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div [ class "mainWide" ] [ viewContents model ]
    }


viewContents : Model -> Html Msg
viewContents model =
    case ( model.maybeCurrentMode, model.maybeCurrentMusicInfo ) of
        ( Just currentMode, Just currentMusicInfo ) ->
            let
                userName =
                    Session.toUser model.session
                        |> Maybe.map User.toUserName
                        |> Maybe.withDefault ""

                pictureUrl =
                    Session.toUser model.session
                        |> Maybe.map User.toPictureUrl
                        |> Maybe.withDefault ""
            in
            div [ class "home_contentsContainer" ]
                -- 左側
                [ div [ class "home_leftContentsContainer" ]
                    [ div [ class "home_leftContents" ]
                        [ div [ class "homeUserSetting_container" ]
                            [ div [ class "homeUserSetting_leftContents" ]
                                [ img
                                    [ class "homeUserSetting_userIcon"
                                    , src pictureUrl
                                    ]
                                    []
                                , div
                                    [ class "homeUserSetting_userNameText" ]
                                    [ text userName ]
                                , img
                                    [ class "homeUserSetting_logoutIcon"
                                    , src "./img/icon_logout.png"
                                    , onClick SignOut
                                    ]
                                    []
                                ]
                            , div [ class "homeUserSetting_rightContents" ]
                                [ div [ class "homeUserSetting_settingContainer" ]
                                    [ viewSettingItem "ノーツ速度"
                                    , viewSettingItem "BGM Volume"
                                    , viewSettingItem "タップ音 Volume"
                                    , viewSettingItem "システム音 Volume"
                                    ]
                                ]
                            ]
                        , div [ class "homeModeSelectTab_container" ]
                            [ viewModeTabBtn currentMode Mode.normal
                            , viewModeTabBtn currentMode Mode.hard
                            , viewModeTabBtn currentMode Mode.master
                            ]
                        , viewMusicList
                            currentMode
                            currentMusicInfo
                            model.allMusicInfoList
                        ]
                    ]

                -- 右側
                , div [ class "home_rightContentsContainer" ]
                    [ div [ class "home_rightContents" ]
                        [ viewCenterArea currentMusicInfo
                        , viewTopLeftArea currentMusicInfo
                        , viewTopRightArea
                        , viewBottomRightArea1 currentMusicInfo
                        , viewBottomRightArea2 currentMusicInfo
                        , viewBottomRightArea currentMusicInfo
                        ]
                    ]
                ]

        _ ->
            div [ class "home_contentsContainer" ]
                [ Page.viewLoading ]


viewSettingItem : String -> Html Msg
viewSettingItem labelText =
    -- TODO: 設定変更機能を実装する
    div [ class "homeUserSetting_settingItem" ]
        [ text labelText
        , div [ class "homeUserSetting_settingBtnContainer" ]
            [ span [ class "homeUserSetting_settingBtn" ] [ text "◆" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◆" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◆" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◇" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◇" ]
            ]
        ]


viewModeTabBtn : Mode -> Mode -> Html Msg
viewModeTabBtn currentMode mode =
    div
        [ classList
            [ ( "homeModeSelectTab_item", True )
            , ( "is-selecting", mode == currentMode )
            ]
        , onClick <| ChangeMode mode
        ]
        [ text <| Mode.unwrap mode ]


viewMusicList : Mode -> MusicInfo -> AllMusicInfoList -> Html Msg
viewMusicList currentMode currentMusicInfo allMusicInfoList =
    let
        filteredMusicInfoList =
            allMusicInfoList
                |> AllMusicInfoList.filteredMusicInfoListByMode currentMode
    in
    div [ class "homeMusicList_container" ]
        (List.map (viewMusicListItem currentMusicInfo) filteredMusicInfoList)


viewMusicListItem : MusicInfo -> MusicInfo -> Html Msg
viewMusicListItem currentMusicInfo musicInfo =
    let
        isSelecting =
            MusicInfo.toMusicName musicInfo == MusicInfo.toMusicName currentMusicInfo
    in
    div
        [ class "homeMusicListItem_container"
        , onClick <| ChangeMusic musicInfo
        ]
        [ div []
            [ div
                [ classList
                    [ ( "homeMusicListItem_top", True )
                    , ( "is-selecting", isSelecting )
                    ]
                ]
                [ div [ class "homeMusicListItem_topText" ]
                    [ text <| MusicInfo.toMusicName musicInfo ]
                ]
            , div
                [ classList
                    [ ( "homeMusicListItem_topTail", True )
                    , ( "is-selecting", isSelecting )
                    ]
                ]
                []
            , div [ class "homeMusicListItem_rankBox combo" ]
                [ div [ class "homeMusicListItem_rankBoxBack combo" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "COMBO" ]

                -- TODO: クリア実績を代入する
                , div [ class "homeMusicListItem_rankText" ] [ text "SSS" ]
                ]
            , div [ class "homeMusicListItem_rankBox score" ]
                [ div [ class "homeMusicListItem_rankBoxBack score" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "SCORE" ]

                -- TODO: クリア実績を代入する
                , div [ class "homeMusicListItem_rankText" ] [ text "SSS" ]
                ]
            ]
        , div [ class "homeMusicListItem_bottomText" ]
            [ text <| MusicInfo.toStringLevel musicInfo ]
        , div [ class "homeMusicListItem_bottomLine" ]
            [ div [ class "homeMusicListItem_bottomLineTail" ] [] ]
        ]


viewCenterArea : MusicInfo -> Html msg
viewCenterArea currentMusicInfo =
    div [ class "home_centerArea", id "home_centerArea" ]
        [ div [ class "homeCenterArea_Inner" ] []
        , div [ class "homeCenterArea_ContentsContainer" ]
            [ div [ class "homeCenterArea_centerWideLine" ] []
            , div [ class "homeCenterArea_centerWideLineLeft" ] []
            , div [ class "homeCenterArea_centerWideLineRight" ] []

            -- 曲名
            , div [ class "homeCenterArea_musicName" ]
                [ text <| MusicInfo.toMusicName currentMusicInfo ]

            -- 作曲者名
            , div [ class "homeCenterArea_composer" ]
                [ text <| MusicInfo.toComposer currentMusicInfo ]

            -- 難易度
            , div [ class "homeCenterArea_levelText" ]
                [ text <| MusicInfo.toStringLevel currentMusicInfo ]

            -- Bpm
            , div [ class "homeCenterArea_box left" ] []
            , div [ class "homeCenterArea_boxLabel left" ]
                [ text "BPM" ]
            , div [ class "homeCenterArea_boxText left" ]
                [ text <| String.fromInt (MusicInfo.toBpm currentMusicInfo) ]

            -- 曲の長さ
            , div [ class "homeCenterArea_box center" ] []
            , div [ class "homeCenterArea_boxLabel center" ]
                [ text "曲の長さ" ]
            , div [ class "homeCenterArea_boxText center" ]
                [ text <| MusicInfo.toStringTime (MusicInfo.toFullTime currentMusicInfo) ]

            -- プレイ回数
            -- TODO: クリア実績を代入する
            , div [ class "homeCenterArea_box right" ] []
            , div [ class "homeCenterArea_boxLabel right" ]
                [ text "プレイ回数" ]
            , div [ class "homeCenterArea_boxText right" ]
                [ text "10回" ]

            -- ランク内訳
            , div [ class "homeCenterArea_rankText title" ]
                [ text ""
                , div [ class "homeCenterArea_rankTitleText score" ] [ text "SCORE" ]
                , div [ class "homeCenterArea_rankTitleText combo" ] [ text "COMBO" ]
                ]
            , div []
                (Rank.allRankList
                    |> List.map (viewCenterAreaRankDetail currentMusicInfo)
                )
            , div [ class "homeCenterArea_rankCenterLine1" ] []
            , div [ class "homeCenterArea_rankCenterLine2" ] []
            ]
        ]


viewCenterAreaRankDetail : MusicInfo -> Rank -> Html msg
viewCenterAreaRankDetail currentMusicInfo rank =
    let
        maxScore =
            MusicInfo.toMaxScore currentMusicInfo

        maxCombo =
            MusicInfo.toMaxCombo currentMusicInfo

        classStr =
            "rank-" ++ Rank.toString rank
    in
    div [ class "homeCenterArea_rankText", class classStr ]
        [ text <| Rank.toString rank
        , div [ class "homeCenterArea_rankDetailText score" ]
            [ text <| String.fromInt (Rank.boundaryScore maxScore rank) ]
        , div [ class "homeCenterArea_rankDetailText combo" ]
            [ text <| String.fromInt (Rank.boundaryCombo maxCombo rank) ]
        , div [ class "homeCenterArea_rankLine", class classStr ] []
        ]


viewTopLeftArea : MusicInfo -> Html msg
viewTopLeftArea currentMusicInfo =
    div [ class "home_topLeftArea", id "home_topLeftArea" ]
        [ div [ class "homeTopLeft_modeText" ]
            [ text <| Mode.unwrap (MusicInfo.toMode currentMusicInfo) ]
        ]


viewTopRightArea : Html msg
viewTopRightArea =
    -- TODO: ランキングデータを代入する
    div [ class "home_topRightArea", id "home_topRightArea" ]
        [ div [ class "homeTopRight_title" ] [ text "楽曲スコアランキング" ]
        , img [ class "homeTopRight_rankIcon first", src "./img/icon_rank_first.png" ] []
        , div [ class "homeTopRight_userNameText first" ] [ text "ほげさん" ]
        , div [ class "homeTopRight_scoreText first" ] [ text "9999999" ]
        , div [ class "homeTopRight_line first" ] []
        , img [ class "homeTopRight_rankIcon second", src "./img/icon_rank_second.png" ] []
        , div [ class "homeTopRight_userNameText second" ] [ text "ふーさん" ]
        , div [ class "homeTopRight_scoreText second" ] [ text "9999999" ]
        , div [ class "homeTopRight_line second" ] []
        , img [ class "homeTopRight_rankIcon third", src "./img/icon_rank_third.png" ] []
        , div [ class "homeTopRight_userNameText third" ] [ text "ばーさん" ]
        , div [ class "homeTopRight_scoreText third" ] [ text "9999999" ]
        , div [ class "homeTopRight_line third" ] []
        ]


viewBottomRightArea1 : MusicInfo -> Html msg
viewBottomRightArea1 currentMusicInfo =
    let
        maxCombo =
            MusicInfo.toMaxCombo currentMusicInfo
    in
    div [ class "home_bottomLeftArea1", id "home_bottomLeftArea1" ]
        [ div [ class "homeBottomLeftArea1_label" ] [ text "COMBO" ]

        -- TODO: クリア実績を代入する
        , div [ class "homeBottomLeftArea1_rankText" ]
            [ text "SSS" ]

        -- TODO: クリア実績を代入する
        , div [ class "homeBottomLeftArea1_bestText" ]
            [ text "9999" ]
        , div [ class "homeBottomLeftArea1_maxText" ]
            [ text <| "/ " ++ String.fromInt maxCombo ]
        ]


viewBottomRightArea2 : MusicInfo -> Html msg
viewBottomRightArea2 currentMusicInfo =
    let
        maxScore =
            MusicInfo.toMaxScore currentMusicInfo
    in
    div [ class "home_bottomLeftArea2", id "home_bottomLeftArea2" ]
        [ div [ class "homeBottomLeftArea2_label" ] [ text "SCORE" ]

        -- TODO: クリア実績を代入する
        , div [ class "homeBottomLeftArea2_rankText" ]
            [ text "SSS" ]

        -- TODO: クリア実績を代入する
        , div [ class "homeBottomLeftArea2_bestText" ]
            [ text "9999999" ]
        , div [ class "homeBottomLeftArea2_maxText" ]
            [ text <| "/ " ++ String.fromInt maxScore ]
        ]


viewBottomRightArea : MusicInfo -> Html msg
viewBottomRightArea currentMusicInfo =
    div []
        [ a [ Route.href <| Route.Play <| MusicInfo.toCsvFileName currentMusicInfo ]
            [ div [ class "home_bottomRightArea", id "home_bottomRightArea" ]
                [ div [ class "homeBottomRight_playText" ] [ text "Play" ]
                ]
            ]
        , div [ class "homeBottomRight_transparentCover1" ] []
        , div [ class "homeBottomRight_transparentCover2" ] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotAllMusicInfoList GotAllMusicInfoList
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
