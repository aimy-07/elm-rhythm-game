port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import AllMusicInfoList exposing (AllMusicInfoList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.CsvFileName as CsvFileName exposing (CsvFileName)
import MusicInfo.Level as Level
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import OwnRecord exposing (OwnRecord, OwnRecordDto)
import Page
import PublicRecord
import Rank exposing (Rank)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , maybeCurrentMusicId : Maybe MusicId
    , maybeCurrentMode : Maybe Mode
    , maybeOwnRecords : Maybe (List OwnRecord)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        allMusicInfoList =
            Session.toAllMusicInfoList session

        getOwnBestRecordsCmd =
            Session.toUser session
                |> Maybe.map (\user -> getOwnBestRecords user.uid)
                |> Maybe.withDefault Cmd.none

        getCurrentCsvFileNameCmd =
            Session.toUser session
                |> Maybe.map (\user -> getCurrentCsvFileName user.uid)
                |> Maybe.withDefault Cmd.none
    in
    ( { session = session
      , maybeCurrentMusicId = Nothing
      , maybeCurrentMode = Nothing
      , maybeOwnRecords = Nothing
      }
    , Cmd.batch
        [ getAllMusicInfoList ()
            |> Page.cmdIf (not <| AllMusicInfoList.isLoaded allMusicInfoList)
        , getOwnBestRecordsCmd
        , getCurrentCsvFileNameCmd
        , startHomeMusic ()
        ]
    )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)
    | GotOwnBestRecords (List OwnRecordDto)
    | GotCurrentCsvFileName CsvFileName
    | ChangeMode Mode
    | ChangeMusic MusicId
    | SignOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllMusicInfoList musicInfoDtos ->
            let
                session =
                    Session.updateAllMusicInfoList musicInfoDtos model.session
            in
            ( { model | session = session }, Cmd.none )

        GotOwnBestRecords ownRecordDto ->
            let
                ownRecords =
                    List.map OwnRecord.new ownRecordDto
            in
            ( { model | maybeOwnRecords = Just ownRecords }, Cmd.none )

        GotCurrentCsvFileName csvFileName ->
            ( { model
                | maybeCurrentMusicId = Just (CsvFileName.toMusicId csvFileName)
                , maybeCurrentMode = Just (CsvFileName.toMode csvFileName)
              }
            , Cmd.none
            )

        ChangeMode mode ->
            let
                maybeUid =
                    Session.toUser model.session
                        |> Maybe.map .uid

                saveCurrentCsvFileNameCmd =
                    case ( model.maybeCurrentMusicId, maybeUid ) of
                        ( Just currentMusicId, Just uid ) ->
                            saveCurrentCsvFileName
                                { uid = uid
                                , csvFileName = CsvFileName.create currentMusicId mode
                                }

                        _ ->
                            Cmd.none
            in
            ( { model | maybeCurrentMode = Just mode }
            , Cmd.batch
                [ saveCurrentCsvFileNameCmd
                , playMusicSelectAnim ()
                ]
            )

        ChangeMusic musicId ->
            let
                maybeUid =
                    Session.toUser model.session
                        |> Maybe.map .uid

                saveCurrentCsvFileNameCmd =
                    case ( model.maybeCurrentMode, maybeUid ) of
                        ( Just currentMode, Just uid ) ->
                            saveCurrentCsvFileName
                                { uid = uid
                                , csvFileName = CsvFileName.create musicId currentMode
                                }

                        _ ->
                            Cmd.none
            in
            ( { model | maybeCurrentMusicId = Just musicId }
            , Cmd.batch
                [ saveCurrentCsvFileNameCmd
                , playMusicSelectAnim ()
                ]
            )

        SignOut ->
            ( model, signOut () )



-- PORT


port getAllMusicInfoList : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg


port getOwnBestRecords : String -> Cmd msg


port gotOwnBestRecords : (List OwnRecordDto -> msg) -> Sub msg


port getCurrentCsvFileName : String -> Cmd msg


port gotCurrentCsvFileName : (String -> msg) -> Sub msg


port saveCurrentCsvFileName : { uid : String, csvFileName : String } -> Cmd msg


port playMusicSelectAnim : () -> Cmd msg


port startHomeMusic : () -> Cmd msg


port signOut : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotAllMusicInfoList GotAllMusicInfoList
        , gotOwnBestRecords GotOwnBestRecords
        , gotCurrentCsvFileName GotCurrentCsvFileName
        ]



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content = div [ class "mainWide" ] [ viewContents model ]
    }


viewContents : Model -> Html Msg
viewContents model =
    let
        allMusicInfoList =
            Session.toAllMusicInfoList model.session

        csvFileName =
            case ( model.maybeCurrentMusicId, model.maybeCurrentMode ) of
                ( Just currentMusicId, Just currentMode ) ->
                    CsvFileName.create currentMusicId currentMode

                _ ->
                    ""

        maybeCurrentMusicInfo =
            AllMusicInfoList.findByCsvFileName csvFileName allMusicInfoList
    in
    case ( maybeCurrentMusicInfo, model.maybeCurrentMode, model.maybeOwnRecords ) of
        ( Just currentMusicInfo, Just currentMode, Just ownRecords ) ->
            let
                userName =
                    Session.toUser model.session
                        |> Maybe.map .userName
                        |> Maybe.withDefault ""

                pictureUrl =
                    Session.toUser model.session
                        |> Maybe.map .pictureUrl
                        |> Maybe.withDefault ""

                maybeCurrentOwnRecord =
                    OwnRecord.findByCsvFileName currentMusicInfo.csvFileName ownRecords
            in
            div [ class "home_contentsContainer" ]
                -- 左側
                [ div
                    [ class "home_leftContentsContainer" ]
                    [ div
                        [ class "home_leftContents" ]
                        [ div
                            [ class "homeUserSetting_container" ]
                            [ div
                                [ class "homeUserSetting_leftContents" ]
                                [ img [ class "homeUserSetting_userIcon", src pictureUrl ] []
                                , div [ class "homeUserSetting_userNameText" ] [ text userName ]
                                , img
                                    [ class "homeUserSetting_logoutIcon"
                                    , src "./img/icon_logout.png"
                                    , onClick SignOut
                                    ]
                                    []
                                ]
                            , div
                                [ class "homeUserSetting_rightContents" ]
                                [ div
                                    [ class "homeUserSetting_settingContainer" ]
                                    [ viewSettingItem "ノーツ速度"
                                    , viewSettingItem "BGM Volume"
                                    , viewSettingItem "タップ音 Volume"
                                    , viewSettingItem "システム音 Volume"
                                    ]
                                ]
                            ]
                        , div
                            [ class "homeModeSelectTab_container" ]
                            [ viewModeTabBtn currentMode Mode.normal
                            , viewModeTabBtn currentMode Mode.hard
                            , viewModeTabBtn currentMode Mode.master
                            ]
                        , viewMusicList currentMode currentMusicInfo allMusicInfoList ownRecords
                        ]
                    ]

                -- 右側
                , div
                    [ class "home_rightContentsContainer" ]
                    [ div
                        [ class "home_rightContents" ]
                        [ viewCenterArea currentMusicInfo maybeCurrentOwnRecord
                        , viewTopLeftArea currentMusicInfo
                        , viewTopRightArea currentMusicInfo
                        , viewBottomLeftArea1 currentMusicInfo maybeCurrentOwnRecord
                        , viewBottomLeftArea2 currentMusicInfo maybeCurrentOwnRecord
                        , viewBottomRightArea currentMusicInfo
                        ]
                    ]
                , div [] [ Page.viewLoaded ]
                ]

        _ ->
            div [ class "home_contentsContainer" ] [ Page.viewLoading ]


viewSettingItem : String -> Html Msg
viewSettingItem labelText =
    -- TODO: 設定変更機能を実装する
    div [ class "homeUserSetting_settingItem" ]
        [ text labelText
        , div
            [ class "homeUserSetting_settingBtnContainer" ]
            [ span [ class "homeUserSetting_settingBtn" ] [ text "◆" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◆" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◆" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◇" ]
            , span [ class "homeUserSetting_settingBtn" ] [ text "◇" ]
            ]
        ]


viewModeTabBtn : Mode -> Mode -> Html Msg
viewModeTabBtn currentMode mode =
    let
        clsIsSelecting =
            if mode == currentMode then
                "is-selecting"

            else
                ""
    in
    div
        [ class "homeModeSelectTab_item"
        , class clsIsSelecting
        , onClick <| ChangeMode mode
        ]
        [ text <| Mode.toString mode ]


viewMusicList : Mode -> MusicInfo -> AllMusicInfoList -> List OwnRecord -> Html Msg
viewMusicList currentMode currentMusicInfo allMusicInfoList ownRecords =
    let
        filteredMusicInfoList =
            allMusicInfoList
                |> AllMusicInfoList.filterByMode currentMode
    in
    div [ class "homeMusicList_container" ]
        (filteredMusicInfoList
            |> List.map
                (\musicInfo ->
                    let
                        maybeOwnRecord =
                            OwnRecord.findByCsvFileName musicInfo.csvFileName ownRecords
                    in
                    viewMusicListItem currentMusicInfo musicInfo maybeOwnRecord
                )
        )


viewMusicListItem : MusicInfo -> MusicInfo -> Maybe OwnRecord -> Html Msg
viewMusicListItem currentMusicInfo musicInfo maybeOwnRecord =
    let
        clsIsSelecting =
            if musicInfo.musicName == currentMusicInfo.musicName then
                "is-selecting"

            else
                ""
    in
    div
        [ class "homeMusicListItem_container"
        , onClick <| ChangeMusic musicInfo.musicId
        ]
        [ div
            []
            [ div
                [ class "homeMusicListItem_top", class clsIsSelecting ]
                [ div [ class "homeMusicListItem_topText" ] [ text musicInfo.musicName ]
                ]
            , div [ class "homeMusicListItem_topTail", class clsIsSelecting ] []
            , div
                [ class "homeMusicListItem_rankBox combo" ]
                [ div [ class "homeMusicListItem_rankBoxBack combo" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "COMBO" ]
                , div
                    [ class "homeMusicListItem_rankText" ]
                    [ text <| OwnRecord.toStringComboRank maybeOwnRecord musicInfo.maxCombo ]
                ]
            , div [ class "homeMusicListItem_rankBox score" ]
                [ div [ class "homeMusicListItem_rankBoxBack score" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "SCORE" ]
                , div
                    [ class "homeMusicListItem_rankText" ]
                    [ text <| OwnRecord.toStringScoreRank maybeOwnRecord musicInfo.maxScore ]
                ]
            ]
        , div [ class "homeMusicListItem_bottomText" ] [ text <| Level.toString musicInfo.level ]
        , div [ class "homeMusicListItem_bottomLine" ]
            [ div [ class "homeMusicListItem_bottomLineTail" ] []
            ]
        ]


viewCenterArea : MusicInfo -> Maybe OwnRecord -> Html msg
viewCenterArea currentMusicInfo maybeOwnRecord =
    div [ class "home_centerArea", id "home_centerArea" ]
        [ div [ class "homeCenterArea_Inner" ] []
        , div
            [ class "homeCenterArea_ContentsContainer" ]
            [ div [ class "homeCenterArea_centerWideLine" ] []
            , div [ class "homeCenterArea_centerWideLineLeft" ] []
            , div [ class "homeCenterArea_centerWideLineRight" ] []
            , div [ class "homeCenterArea_musicName" ] [ text currentMusicInfo.musicName ]
            , div [ class "homeCenterArea_composer" ] [ text currentMusicInfo.composer ]
            , div [ class "homeCenterArea_levelText" ] [ text <| Level.toString currentMusicInfo.level ]
            , div [ class "homeCenterArea_box left" ] []
            , div [ class "homeCenterArea_boxLabel left" ] [ text "BPM" ]
            , div [ class "homeCenterArea_boxText left" ] [ text <| String.fromInt currentMusicInfo.bpm ]
            , div [ class "homeCenterArea_box center" ] []
            , div [ class "homeCenterArea_boxLabel center" ] [ text "曲の長さ" ]
            , div [ class "homeCenterArea_boxText center" ] [ text <| MusicInfo.toStringTime currentMusicInfo.fullTime ]
            , div [ class "homeCenterArea_box right" ] []
            , div [ class "homeCenterArea_boxLabel right" ] [ text "プレイ回数" ]
            , div [ class "homeCenterArea_boxText right" ] [ text <| OwnRecord.toPlayCount maybeOwnRecord ++ "回" ]
            , div
                [ class "homeCenterArea_rankText title" ]
                [ div [ class "homeCenterArea_rankTitleText score" ] [ text "SCORE" ]
                , div [ class "homeCenterArea_rankTitleText combo" ] [ text "COMBO" ]
                ]
            , div
                []
                (List.map (viewCenterAreaRankDetail currentMusicInfo) Rank.allRankList)
            , div [ class "homeCenterArea_rankCenterLine1" ] []
            , div [ class "homeCenterArea_rankCenterLine2" ] []
            ]
        ]


viewCenterAreaRankDetail : MusicInfo -> Rank -> Html msg
viewCenterAreaRankDetail currentMusicInfo rank =
    let
        clsRankNum =
            "rank-" ++ Rank.toString rank
    in
    div [ class "homeCenterArea_rankText", class clsRankNum ]
        [ text <| Rank.toString rank
        , div
            [ class "homeCenterArea_rankDetailText score" ]
            [ text <| String.fromInt (Rank.boundaryScore currentMusicInfo.maxScore rank) ]
        , div
            [ class "homeCenterArea_rankDetailText combo" ]
            [ text <| String.fromInt (Rank.boundaryCombo currentMusicInfo.maxCombo rank) ]
        , div
            [ class "homeCenterArea_rankLine", class clsRankNum ]
            []
        ]


viewTopLeftArea : MusicInfo -> Html msg
viewTopLeftArea currentMusicInfo =
    div [ class "home_topLeftArea", id "home_topLeftArea" ]
        [ div [ class "homeTopLeft_modeText" ] [ text <| Mode.toString currentMusicInfo.mode ]
        ]


viewTopRightArea : MusicInfo -> Html msg
viewTopRightArea currentMusicInfo =
    div [ class "home_topRightArea", id "home_topRightArea" ]
        [ div [ class "homeTopRight_title" ] [ text "楽曲スコアランキング" ]
        , img [ class "homeTopRight_rankIcon first", src "./img/icon_rank_first.png" ] []
        , div
            [ class "homeTopRight_userNameText first" ]
            [ text <| PublicRecord.toStringUserName (MusicInfo.toFirstRecord currentMusicInfo) ]
        , div
            [ class "homeTopRight_scoreText first" ]
            [ text <| PublicRecord.toStringBestScore (MusicInfo.toFirstRecord currentMusicInfo) ]
        , div [ class "homeTopRight_line first" ] []
        , img [ class "homeTopRight_rankIcon second", src "./img/icon_rank_second.png" ] []
        , div
            [ class "homeTopRight_userNameText second" ]
            [ text <| PublicRecord.toStringUserName (MusicInfo.toSecondRecord currentMusicInfo) ]
        , div
            [ class "homeTopRight_scoreText second" ]
            [ text <| PublicRecord.toStringBestScore (MusicInfo.toSecondRecord currentMusicInfo) ]
        , div [ class "homeTopRight_line second" ] []
        , img [ class "homeTopRight_rankIcon third", src "./img/icon_rank_third.png" ] []
        , div
            [ class "homeTopRight_userNameText third" ]
            [ text <| PublicRecord.toStringUserName (MusicInfo.toThirdRecord currentMusicInfo) ]
        , div
            [ class "homeTopRight_scoreText third" ]
            [ text <| PublicRecord.toStringBestScore (MusicInfo.toThirdRecord currentMusicInfo) ]
        , div [ class "homeTopRight_line third" ] []
        ]


viewBottomLeftArea1 : MusicInfo -> Maybe OwnRecord -> Html msg
viewBottomLeftArea1 currentMusicInfo maybeOwnRecord =
    div [ class "home_bottomLeftArea1", id "home_bottomLeftArea1" ]
        [ div [ class "homeBottomLeftArea1_label" ] [ text "COMBO" ]
        , div
            [ class "homeBottomLeftArea1_rankText" ]
            [ text <| OwnRecord.toStringComboRank maybeOwnRecord currentMusicInfo.maxCombo ]
        , div
            [ class "homeBottomLeftArea1_bestText" ]
            [ text <| OwnRecord.toStringCombo maybeOwnRecord ]
        , div
            [ class "homeBottomLeftArea1_maxText" ]
            [ text <| "/ " ++ String.fromInt currentMusicInfo.maxCombo ]
        ]


viewBottomLeftArea2 : MusicInfo -> Maybe OwnRecord -> Html msg
viewBottomLeftArea2 currentMusicInfo maybeOwnRecord =
    div [ class "home_bottomLeftArea2", id "home_bottomLeftArea2" ]
        [ div [ class "homeBottomLeftArea2_label" ] [ text "SCORE" ]
        , div
            [ class "homeBottomLeftArea2_rankText" ]
            [ text <| OwnRecord.toStringScoreRank maybeOwnRecord currentMusicInfo.maxScore ]
        , div
            [ class "homeBottomLeftArea2_bestText" ]
            [ text <| OwnRecord.toStringScore maybeOwnRecord ]
        , div
            [ class "homeBottomLeftArea2_maxText" ]
            [ text <| "/ " ++ String.fromInt currentMusicInfo.maxScore ]
        ]


viewBottomRightArea : MusicInfo -> Html msg
viewBottomRightArea currentMusicInfo =
    div []
        [ a [ Route.href <| Route.Play currentMusicInfo.csvFileName ]
            [ div
                [ class "home_bottomRightArea", id "home_bottomRightArea" ]
                [ div [ class "homeBottomRight_playText" ] [ text "Play" ]
                ]
            ]
        , div [ class "homeBottomRight_transparentCover1" ] []
        , div [ class "homeBottomRight_transparentCover2" ] []
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
