module Page.Home exposing
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
import AnimationManager
import AudioManager exposing (playSE)
import AudioManager.SE as SE
import Constants exposing (allMode)
import Html exposing (Html, a, div, img, input, label, text)
import Html.Attributes exposing (class, disabled, href, id, max, min, name, src, step, target, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import MusicInfo as MusicInfo exposing (MusicInfo)
import MusicInfo.CsvFileName as CsvFileName
import MusicInfo.Level as Level
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import OwnRecord exposing (OwnRecord, OwnRecordDto)
import Page
import Page.Home.UserSettingPanelS as UserSettingPanelS exposing (UserSettingPanelS(..))
import PublicRecord exposing (PublicRecord, PublicRecordDto)
import Rank exposing (Rank)
import Route
import Session exposing (Session)
import Setting exposing (Setting, SettingDto)
import Setting.NotesSpeed as NotesSpeed
import Setting.Volume as Volume
import User exposing (User)
import UserSetting exposing (UserSetting)
import Utils exposing (viewIf)



-- MODEL


type alias Model =
    { session : Session
    , allMusicInfoList : AllMusicInfoList
    , maybeOwnRecords : Maybe (List OwnRecord)
    , maybePublicRecords : Maybe (List PublicRecord)
    , userSetting : UserSetting
    , userSettingPanelS : UserSettingPanelS
    , pictureUploadS : PictureUploadS
    }


type PictureUploadS
    = NotUploading
    | Uploading


init : Session -> AllMusicInfoList -> ( Model, Cmd Msg )
init session allMusicInfoList =
    case Session.toUser session of
        Just user ->
            ( initModel session allMusicInfoList
            , Cmd.batch
                [ OwnRecord.getOwnRecords user.uid
                , PublicRecord.getPublicRecords ()
                , UserSetting.getUserSetting user.uid
                ]
            )

        Nothing ->
            -- Homeで user == Nothing にはまずならないが、念のためエラー画面に飛ばす処理を入れておく
            let
                navKey =
                    Session.toNavKey session
            in
            ( initModel (Session.init navKey) allMusicInfoList
            , Route.replaceUrl navKey Route.Error
            )


initModel : Session -> AllMusicInfoList -> Model
initModel session allMusicInfoList =
    { session = session
    , allMusicInfoList = allMusicInfoList
    , maybeOwnRecords = Nothing
    , maybePublicRecords = Nothing
    , userSetting = UserSetting.init
    , userSettingPanelS = UserSettingPanelS.init
    , pictureUploadS = NotUploading
    }



-- UPDATE


type Msg
    = GotOwnRecords (List OwnRecordDto)
    | GotPublicRecords (List PublicRecordDto)
    | GotUserSetting SettingDto
    | ChangeMusicId MusicId
    | ChangeMode Mode
    | ChangeNotesSpeed String
    | ChangeBgmVolume String
    | ChangeSeVolume String
    | InputUserName String
    | SelectdPicture Decode.Value
    | SavedUserPicture String
    | ClickedSettingPanelShowBtn
    | ClickedInfoPanelShowBtn
    | ClickedPanelCloseBtn
    | SignOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Session.toUser model.session of
        Just user ->
            case msg of
                GotOwnRecords ownRecordDtos ->
                    let
                        ownRecords =
                            List.map OwnRecord.new ownRecordDtos
                    in
                    ( { model | maybeOwnRecords = Just ownRecords }, Cmd.none )

                GotPublicRecords publicRecordDtos ->
                    let
                        publicRecords =
                            List.map PublicRecord.new publicRecordDtos
                    in
                    ( { model | maybePublicRecords = Just publicRecords }, Cmd.none )

                GotUserSetting settingDto ->
                    let
                        userSetting =
                            UserSetting.new settingDto

                        currentMusicId =
                            UserSetting.toSetting userSetting
                                |> Maybe.map .currentMusicId
                                |> Maybe.withDefault ""

                        audioUrl =
                            model.allMusicInfoList
                                |> AllMusicInfoList.toAudioInfoFindByMusicId currentMusicId
                                |> Maybe.map .audioUrl

                        bgmVolume =
                            UserSetting.toSetting userSetting
                                |> Maybe.map .bgmVolume
                    in
                    ( { model | userSetting = userSetting }
                    , AudioManager.playBGM audioUrl bgmVolume True
                    )

                ChangeMusicId musicId ->
                    let
                        updatedUserSetting =
                            UserSetting.updateCurrentMusicId musicId model.userSetting

                        audioUrl =
                            model.allMusicInfoList
                                |> AllMusicInfoList.toAudioInfoFindByMusicId musicId
                                |> Maybe.map .audioUrl

                        bgmVolume =
                            UserSetting.toSetting updatedUserSetting
                                |> Maybe.map .bgmVolume

                        seVolume =
                            UserSetting.toSetting updatedUserSetting
                                |> Maybe.map .seVolume
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveCurrentMusicId { uid = user.uid, currentMusicId = musicId }
                        , AudioManager.playBGM audioUrl bgmVolume True
                        , AnimationManager.playMusicSelectAnim ()
                        , playSE SE.MusicSelect seVolume
                        ]
                    )

                ChangeMode mode ->
                    let
                        updatedUserSetting =
                            UserSetting.updateCurrentMode mode model.userSetting

                        seVolume =
                            UserSetting.toSetting updatedUserSetting
                                |> Maybe.map .seVolume
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveCurrentMode { uid = user.uid, currentMode = Mode.unwrap mode }
                        , AnimationManager.playMusicSelectAnim ()
                        , playSE SE.MusicSelect seVolume
                        ]
                    )

                ChangeNotesSpeed speedValue ->
                    let
                        notesSpeed =
                            NotesSpeed.fromString speedValue

                        updatedUserSetting =
                            UserSetting.updateNotesSpeed notesSpeed model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Setting.saveNotesSpeed { uid = user.uid, notesSpeed = notesSpeed }
                    )

                ChangeBgmVolume volumeValue ->
                    let
                        bgmVolume =
                            Volume.fromString volumeValue

                        updatedUserSetting =
                            UserSetting.updateBgmVolume bgmVolume model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveBgmVolume { uid = user.uid, bgmVolume = bgmVolume }
                        , AudioManager.changeBgmVolume bgmVolume
                        ]
                    )

                ChangeSeVolume volumeValue ->
                    let
                        seVolume =
                            Volume.fromString volumeValue

                        updatedUserSetting =
                            UserSetting.updateSeVolume seVolume model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Setting.saveSeVolume { uid = user.uid, seVolume = seVolume }
                    )

                InputUserName nextUserName ->
                    let
                        updatedSession =
                            Session.updateUser nextUserName User.updateUserName model.session
                    in
                    ( { model | session = updatedSession }
                    , User.saveUserName { uid = user.uid, userName = nextUserName }
                    )

                SelectdPicture event ->
                    ( { model | pictureUploadS = Uploading }
                    , User.saveUserPicture { uid = user.uid, event = event }
                    )

                SavedUserPicture url ->
                    let
                        updatedSession =
                            Session.updateUser url User.updatePictureUrl model.session
                    in
                    ( { model | pictureUploadS = NotUploading, session = updatedSession }, Cmd.none )

                ClickedSettingPanelShowBtn ->
                    ( { model | userSettingPanelS = SettingShow }, Cmd.none )

                ClickedInfoPanelShowBtn ->
                    ( { model | userSettingPanelS = InfoShow }, Cmd.none )

                ClickedPanelCloseBtn ->
                    ( { model | userSettingPanelS = UserSettingPanelS.close model.userSettingPanelS }, Cmd.none )

                SignOut ->
                    ( model, Session.signOut () )

        Nothing ->
            -- Homeで user == Nothing にはまずならないが、念のためエラー画面に飛ばす処理を入れておく
            let
                navKey =
                    Session.toNavKey model.session
            in
            ( { model | session = Session.init navKey }
            , Route.replaceUrl navKey Route.Error
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ OwnRecord.gotOwnRecords GotOwnRecords
        , PublicRecord.gotPublicRecords GotPublicRecords
        , UserSetting.gotUserSetting GotUserSetting
        , User.savedUserPicture SavedUserPicture
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
        maybeUser =
            Session.toUser model.session

        maybeSetting =
            UserSetting.toSetting model.userSetting

        currentCsvFileName =
            maybeSetting
                |> Maybe.map (\setting -> CsvFileName.new setting.currentMusicId setting.currentMode)
                |> Maybe.withDefault ""

        maybeCurrentMusicInfo =
            AllMusicInfoList.toMusicInfoFindByCsvFileName currentCsvFileName model.allMusicInfoList
    in
    case ( maybeUser, maybeSetting ) of
        ( Just user, Just setting ) ->
            case ( maybeCurrentMusicInfo, model.maybeOwnRecords, model.maybePublicRecords ) of
                ( Just currentMusicInfo, Just ownRecords, Just publicRecords ) ->
                    let
                        maybeCurrentOwnRecord =
                            OwnRecord.findByCsvFileName currentMusicInfo.csvFileName ownRecords

                        maybeCurrentPublicRecord =
                            PublicRecord.findByCsvFileName currentMusicInfo.csvFileName publicRecords
                    in
                    div
                        [ class "home_contentsContainer" ]
                        [ div
                            [ class "home_contents" ]
                            -- 左側
                            [ div
                                [ class "home_leftContents" ]
                                [ div
                                    [ class "homeUserSetting_container" ]
                                    [ viewUser user model.pictureUploadS model.userSettingPanelS
                                    , viewUserSettingPanel setting model.userSettingPanelS
                                    ]
                                , viewSettingIcon
                                , viewInfoIcon
                                , viewLogoutIcon
                                , viewModeTab setting
                                , viewMusicList setting.currentMode currentMusicInfo model.allMusicInfoList ownRecords
                                ]

                            -- 右側
                            , div
                                [ class "home_rightContents" ]
                                [ viewCenterArea currentMusicInfo maybeCurrentOwnRecord
                                , viewTopLeftArea currentMusicInfo
                                , viewTopRightArea maybeCurrentPublicRecord user
                                , viewBottomLeftArea1 currentMusicInfo maybeCurrentOwnRecord
                                , viewBottomLeftArea2 currentMusicInfo maybeCurrentOwnRecord
                                , viewBottomRightArea currentMusicInfo
                                ]
                            ]
                        , div [] [ Page.viewLoaded ]
                        ]

                _ ->
                    div [ class "home_contentsContainer" ] [ Page.viewLoading ]

        _ ->
            div [ class "home_contentsContainer" ] [ Page.viewLoading ]


viewSettingIcon : Html Msg
viewSettingIcon =
    img [ class "homeUserSetting_settingIcon", src "./img/icon_setting.png", onClick ClickedSettingPanelShowBtn ] []


viewInfoIcon : Html Msg
viewInfoIcon =
    img [ class "homeUserSetting_infoIcon", src "./img/icon_info.png", onClick ClickedInfoPanelShowBtn ] []


viewLogoutIcon : Html Msg
viewLogoutIcon =
    img [ class "homeUserSetting_logoutIcon", src "./img/icon_logout.png", onClick SignOut ] []


viewUser : User -> PictureUploadS -> UserSettingPanelS -> Html Msg
viewUser user pictureUploadS userSettingPanelS =
    let
        isUploading =
            pictureUploadS == Uploading

        clsIsUploading =
            if isUploading then
                "is-uploading"

            else
                ""
    in
    div
        [ class "homeUserSetting_userInfoContents"
        , class <| UserSettingPanelS.clsStr userSettingPanelS
        ]
        [ img [ class "homeUserSetting_userIcon", src user.pictureUrl ] []
        , label
            [ class "homeUserSetting_userIconBtnLabel"
            , class clsIsUploading
            ]
            [ input
                [ class "homeUserSetting_userIconBtn"
                , type_ "file"
                , name "userPictureUrl"
                , on "change" (Decode.map SelectdPicture Decode.value)
                , disabled isUploading
                ]
                []
            , img [ class "homeUserSetting_userIconBtnImage", src "./img/icon_photo.png" ] []
                |> viewIf (not isUploading)
            , img [ class "homeUserSetting_userIconBtnImage", class clsIsUploading, src "./img/icon_loading.png" ] []
                |> viewIf isUploading
            ]
        , input [ class "homeUserSetting_userNameInput", value user.userName, onInput InputUserName ] []
        ]


viewUserSettingPanel : Setting -> UserSettingPanelS -> Html Msg
viewUserSettingPanel setting userSettingPanelS =
    div
        [ class "homeUserSetting_userSettingPanelContents" ]
        [ div
            [ class "homeUserSettingPanel_outer" ]
            [ img
                [ class "homeUserSettingPanel_close"
                , src "./img/icon_close.png"
                , onClick ClickedPanelCloseBtn
                ]
                []
            , viewSetting setting
                |> viewIf (UserSettingPanelS.isSetting userSettingPanelS)
            , viewInfo
                |> viewIf (UserSettingPanelS.isInfo userSettingPanelS)
            ]
        ]


viewSetting : Setting -> Html Msg
viewSetting setting =
    div [ class "homeUserSettingPanel" ]
        [ div [ class "homeUserSettingPanel_title" ] [ text "Setting" ]
        , viewRangeSlider "ノーツ速度" setting.notesSpeed ChangeNotesSpeed
        , viewRangeSlider "BGM音量" setting.bgmVolume ChangeBgmVolume
        , viewRangeSlider "システム音量" setting.seVolume ChangeSeVolume
        ]


viewRangeSlider : String -> Float -> (String -> Msg) -> Html Msg
viewRangeSlider label value_ msg =
    div []
        [ div [ class "homeUserSetting_rangeSliderLabel" ] [ text label ]
        , input
            [ class "homeUserSetting_rangeSlider"
            , type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "1"
            , step "0.1"
            , value <| String.fromFloat value_
            , sliderStyle value_
            , onInput msg
            ]
            []
        ]


sliderStyle : Float -> Html.Attribute msg
sliderStyle value =
    let
        percentStr =
            String.fromFloat (value * 100) ++ "%"
    in
    Html.Attributes.style
        "background-image"
        ("-webkit-gradient(linear, left top, right top, color-stop(" ++ percentStr ++ ", white), color-stop(" ++ percentStr ++ ", #8f95a3))")


viewInfo : Html msg
viewInfo =
    div [ class "homeUserSettingPanel" ]
        [ div [ class "homeUserSettingPanel_title" ] [ text "Info" ]
        , div
            [ class "homeUserSetting_infoText" ]
            [ a [ href "https://twitter.com/yun_ar_1107", target "_blank" ] [ text "開発者：Yuna Tanaka" ] ]
        , div
            [ class "homeUserSetting_infoText" ]
            [ a [ href "https://github.com/aimy-07/elm-rhythm-game/issues", target "_blank" ] [ text "バグ報告はこちら" ] ]
        ]


viewModeTab : Setting -> Html Msg
viewModeTab setting =
    let
        viewModeTabBtn mode =
            let
                clsIsSelecting =
                    if mode == setting.currentMode then
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
    in
    div [ class "homeModeSelectTab_container" ] (List.map viewModeTabBtn allMode)


viewMusicList : Mode -> MusicInfo -> AllMusicInfoList -> List OwnRecord -> Html Msg
viewMusicList currentMode currentMusicInfo allMusicInfoList ownRecords =
    let
        filteredMusicInfoList =
            allMusicInfoList
                |> AllMusicInfoList.toMusicInfoListFilterByMode currentMode
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
            if musicInfo.musicId == currentMusicInfo.musicId then
                "is-selecting"

            else
                ""
    in
    div
        [ class "homeMusicListItem_container"
        , onClick <| ChangeMusicId musicInfo.musicId
        ]
        [ div
            [ class "homeMusicListItem_topContainer" ]
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
        , div
            [ class "homeMusicListItem_bottomContainer" ]
            [ div [ class "homeMusicListItem_bottomText" ] [ text <| Level.toString musicInfo.level ]
            , div [ class "homeMusicListItem_bottomLine" ]
                [ div [ class "homeMusicListItem_bottomLineTail" ] []
                ]
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


viewTopRightArea : Maybe PublicRecord -> User -> Html msg
viewTopRightArea maybeCurrentPublicRecord user =
    let
        firstRecord =
            PublicRecord.toFirstScoreRecord maybeCurrentPublicRecord

        secondRecord =
            PublicRecord.toSecondScoreRecord maybeCurrentPublicRecord

        thirdRecord =
            PublicRecord.toThirdScoreRecord maybeCurrentPublicRecord

        viewRankingItem clsRank record =
            let
                clsIsMe =
                    if PublicRecord.isOwnRecord record user.uid then
                        "is-Me"

                    else
                        ""
            in
            [ img
                [ class "homeTopRight_rankIcon"
                , class clsRank
                , src <| "./img/icon_rank_" ++ clsRank ++ ".png"
                ]
                []
            , div
                [ class "homeTopRight_userNameText", class clsRank, class clsIsMe ]
                [ text <| PublicRecord.toStringUserName record ]
            , div
                [ class "homeTopRight_scoreText", class clsRank ]
                [ text <| PublicRecord.toStringScore record ]
            , div [ class "homeTopRight_line", class clsRank ] []
            ]
    in
    div
        [ class "home_topRightArea"
        , id "home_topRightArea"
        ]
        (div [ class "homeTopRight_title" ] [ text "楽曲スコアランキング" ]
            :: viewRankingItem "first" firstRecord
            ++ viewRankingItem "second" secondRecord
            ++ viewRankingItem "third" thirdRecord
        )


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
        -- 戻るボタンでHomeに戻ることを許容する
        [ a [ Route.href <| Route.Play (Just <| currentMusicInfo.csvFileName) ]
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


toAllMusicInfoList : Model -> AllMusicInfoList
toAllMusicInfoList model =
    model.allMusicInfoList
