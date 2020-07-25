module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toAllMusicData
    , toAudioLoadingS
    , toSession
    , toUserSetting
    , update
    , view
    )

import AllMusicData exposing (AllMusicData)
import AllMusicData.MusicData as MusicData exposing (MusicData)
import AllMusicData.MusicData.CsvFileName as CsvFileName
import AllMusicData.MusicData.Level as Level
import AllMusicData.MusicData.Mode as Mode exposing (Mode)
import AllMusicData.MusicData.MusicId exposing (MusicId)
import AnimationManager
import AudioManager
import AudioManager.AudioLoadingS exposing (AudioLoadingS)
import AudioManager.BGM as BGM
import AudioManager.SE as SE
import Constants exposing (allModeList, currentMusicIdDefault)
import Html exposing (Html, a, div, img, input, label, span, text)
import Html.Attributes exposing (class, href, id, name, src, step, target, type_, value)
import Html.Events exposing (on, onClick, onInput, onMouseUp)
import Json.Decode as Decode
import OwnRecord exposing (OwnRecordDto)
import Page
import Page.Home.RankingRecords as RankingRecords exposing (RankingRecords)
import Page.Home.RankingRecords.RankingRecord as RankingRecord
import Page.Home.UserPlayRecords as UserPlayRecords exposing (UserPlayRecords)
import Page.Home.UserPlayRecords.UserPlayRecord as UserPlayRecord exposing (UserPlayRecord)
import Page.Home.UserSettingPanelS as UserSettingPanelS exposing (UserSettingPanelS)
import Process
import PublicRecord exposing (PublicRecordDto)
import Rank exposing (Rank)
import Route
import Session exposing (Session)
import Session.User as User exposing (User, UserDto)
import Task
import UserSetting exposing (UserSetting, UserSettingDto)
import UserSetting.Setting as Setting exposing (Setting)
import UserSetting.Setting.NotesSpeed as NotesSpeed
import UserSetting.Setting.Volume as Volume
import Utils exposing (classIf, viewIf)



-- MODEL


type alias Model =
    { session : Session
    , allMusicData : AllMusicData
    , audioLoadingS : AudioLoadingS
    , userPlayRecords : UserPlayRecords
    , rankingRecords : RankingRecords
    , userSetting : UserSetting
    , userSettingPanelS : UserSettingPanelS
    , pictureUploadS : PictureUploadS
    }


type PictureUploadS
    = NotUploading
    | Uploading
    | FailedUpload


init : Session -> AllMusicData -> AudioLoadingS -> ( Model, Cmd Msg )
init session audioMusicData audioLoadingS =
    case Session.toUser session of
        Just user ->
            ( { session = session
              , allMusicData = audioMusicData
              , audioLoadingS = audioLoadingS
              , userPlayRecords = UserPlayRecords.init
              , rankingRecords = RankingRecords.init
              , userSetting = UserSetting.init
              , userSettingPanelS = UserSettingPanelS.init
              , pictureUploadS = NotUploading
              }
            , Cmd.batch
                [ UserPlayRecords.initCmd (User.toUid user)
                , RankingRecords.initCmd
                , UserSetting.getUserSetting (User.toUid user)
                ]
            )

        Nothing ->
            -- Homeで user == Nothing にはまずならないが、念のためエラー画面に飛ばす処理を入れておく
            ( { session = Session.init (Session.toNavKey session)
              , allMusicData = audioMusicData
              , audioLoadingS = audioLoadingS
              , userPlayRecords = UserPlayRecords.init
              , rankingRecords = RankingRecords.init
              , userSetting = UserSetting.init
              , userSettingPanelS = UserSettingPanelS.init
              , pictureUploadS = NotUploading
              }
            , Route.replaceUrl (Session.toNavKey session) Route.Error
            )



-- UPDATE


type Msg
    = GotUserPlayRecords OwnRecordDto
    | GotRankingRecord PublicRecordDto
    | GotRankingUsers (List UserDto)
    | GotUserSetting UserSettingDto
    | ChangeMusicId MusicId
    | ChangeMode Mode
    | ChangeNotesSpeed String
    | ChangeBgmVolume String
    | ChangeSeVolume String
    | InputUserName String
    | SelectedUserPicture Decode.Value
    | CompletedSaveUserPicture String
    | FailedSaveUserPicture ()
    | CompletedFailedAnimation
    | ClickedSettingPanelShowBtn
    | ClickedHelpPanelShowBtn
    | ClickedInfoPanelShowBtn
    | ClickedPanelCloseBtn
    | SignOut
    | PlayPlayBtnSE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Session.toUser model.session of
        Just user ->
            let
                bgmVolume =
                    UserSetting.toSetting model.userSetting
                        |> Maybe.map Setting.toBgmVolume

                seVolume =
                    UserSetting.toSetting model.userSetting
                        |> Maybe.map Setting.toSeVolume
            in
            case msg of
                GotUserPlayRecords ownRecordDtos ->
                    ( { model | userPlayRecords = UserPlayRecords.gotOwnRecord ownRecordDtos model.userPlayRecords }
                    , Cmd.none
                    )

                GotRankingRecord publicRecordDto ->
                    let
                        ( updatedRankingRecords, rankingRecordsCmd ) =
                            RankingRecords.gotPublicRecord publicRecordDto model.rankingRecords
                    in
                    ( { model | rankingRecords = updatedRankingRecords }, rankingRecordsCmd )

                GotRankingUsers userDtos ->
                    ( { model | rankingRecords = RankingRecords.gotUsers userDtos model.rankingRecords }
                    , Cmd.none
                    )

                GotUserSetting settingDto ->
                    let
                        userSetting =
                            UserSetting.new settingDto

                        currentMusicId =
                            UserSetting.toSetting userSetting
                                |> Maybe.map Setting.toCurrentMusicId
                                |> Maybe.withDefault currentMusicIdDefault

                        updatedBgmVolume =
                            UserSetting.toSetting userSetting
                                |> Maybe.map Setting.toBgmVolume
                    in
                    ( { model | userSetting = userSetting }
                    , AudioManager.playBGM (BGM.sampleFromMusicId currentMusicId) updatedBgmVolume
                    )

                ChangeMusicId musicId ->
                    let
                        updatedUserSetting =
                            UserSetting.updateSetting (Setting.updateCurrentMusicId musicId) model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveCurrentMusicId { uid = User.toUid user, currentMusicId = musicId }
                        , AudioManager.playBGM (BGM.sampleFromMusicId musicId) bgmVolume
                        , AudioManager.playSE SE.SelectPlayMusic seVolume
                        , AnimationManager.playMusicSelectAnim ()
                        ]
                    )

                ChangeMode mode ->
                    let
                        updatedUserSetting =
                            UserSetting.updateSetting (Setting.updateCurrentMode mode) model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveCurrentMode { uid = User.toUid user, currentMode = Mode.unwrap mode }
                        , AudioManager.playSE SE.SelectPlayMusic seVolume
                        , AnimationManager.playMusicSelectAnim ()
                        ]
                    )

                ChangeNotesSpeed speedValue ->
                    let
                        notesSpeed =
                            NotesSpeed.fromString speedValue

                        updatedUserSetting =
                            UserSetting.updateSetting (Setting.updateNotesSpeed notesSpeed) model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveNotesSpeed { uid = User.toUid user, notesSpeed = notesSpeed }
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                ChangeBgmVolume volumeValue ->
                    let
                        nextBgmVolume =
                            Volume.fromString volumeValue

                        updatedUserSetting =
                            UserSetting.updateSetting (Setting.updateBgmVolume nextBgmVolume) model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveBgmVolume { uid = User.toUid user, bgmVolume = nextBgmVolume }
                        , AudioManager.changeBgmVolume nextBgmVolume
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                ChangeSeVolume volumeValue ->
                    let
                        nextSeVolume =
                            Volume.fromString volumeValue

                        updatedUserSetting =
                            UserSetting.updateSetting (Setting.updateSeVolume nextSeVolume) model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveSeVolume { uid = User.toUid user, seVolume = nextSeVolume }
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                InputUserName nextUserName ->
                    let
                        updatedSession =
                            Session.updateUser (User.updateName nextUserName) model.session
                    in
                    ( { model | session = updatedSession }
                    , Cmd.batch
                        [ User.saveUserName { uid = User.toUid user, name = nextUserName }
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                SelectedUserPicture event ->
                    ( { model | pictureUploadS = Uploading }
                    , User.saveUserPicture { uid = User.toUid user, event = event }
                    )

                CompletedSaveUserPicture url ->
                    let
                        updatedSession =
                            Session.updateUser (User.updatePictureUrl url) model.session
                    in
                    ( { model | pictureUploadS = NotUploading, session = updatedSession }
                    , AudioManager.playSE SE.Succeed seVolume
                    )

                FailedSaveUserPicture _ ->
                    ( { model | pictureUploadS = FailedUpload }
                    , Cmd.batch
                        [ AudioManager.playSE SE.Attention seVolume
                        , Process.sleep 1500 |> Task.perform (\_ -> CompletedFailedAnimation)
                        ]
                    )

                CompletedFailedAnimation ->
                    ( { model | pictureUploadS = NotUploading }, Cmd.none )

                ClickedSettingPanelShowBtn ->
                    ( { model | userSettingPanelS = UserSettingPanelS.showSetting }
                    , AudioManager.playSE SE.Select seVolume
                    )

                ClickedHelpPanelShowBtn ->
                    ( { model | userSettingPanelS = UserSettingPanelS.showHelp }
                    , AudioManager.playSE SE.Select seVolume
                    )

                ClickedInfoPanelShowBtn ->
                    ( { model | userSettingPanelS = UserSettingPanelS.showInformation }
                    , AudioManager.playSE SE.Select seVolume
                    )

                ClickedPanelCloseBtn ->
                    ( { model | userSettingPanelS = UserSettingPanelS.close model.userSettingPanelS }
                    , AudioManager.playSE SE.Cancel seVolume
                    )

                SignOut ->
                    ( model
                    , Cmd.batch
                        [ Session.signOut ()
                        , AudioManager.playSE SE.Select seVolume
                        ]
                    )

                PlayPlayBtnSE ->
                    ( model, AudioManager.playSE SE.Decision seVolume )

        Nothing ->
            -- Homeで user == Nothing にはまずならないが、念のためエラー画面に飛ばす処理を入れておく
            ( { model | session = Session.init (Session.toNavKey model.session) }
            , Route.replaceUrl (Session.toNavKey model.session) Route.Error
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ OwnRecord.gotOwnRecord GotUserPlayRecords
        , PublicRecord.gotPublicRecord GotRankingRecord
        , User.gotUsers GotRankingUsers
        , UserSetting.gotUserSetting GotUserSetting
        , User.completedSaveUserPicture CompletedSaveUserPicture
        , User.failedSaveUserPicture FailedSaveUserPicture
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        maybeUser =
            Session.toUser model.session

        maybeSetting =
            UserSetting.toSetting model.userSetting
    in
    case ( maybeUser, maybeSetting ) of
        ( Just user, Just setting ) ->
            let
                currentCsvFileName =
                    CsvFileName.create (Setting.toCurrentMusicId setting) (Setting.toCurrentMode setting)

                maybeCurrentMusicData =
                    AllMusicData.findByCsvFileName currentCsvFileName model.allMusicData

                isLoadedUserPlayRecords =
                    UserPlayRecords.isLoaded model.userPlayRecords

                isLoadedRankingRecords =
                    RankingRecords.isLoaded model.rankingRecords
            in
            case ( maybeCurrentMusicData, isLoadedUserPlayRecords, isLoadedRankingRecords ) of
                ( Just currentMusicData, True, True ) ->
                    div [ class "home_back" ]
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
                                , viewHelpIcon
                                , viewInfoIcon
                                , viewLogoutIcon
                                , viewModeTab setting
                                , viewMusicList
                                    (Setting.toCurrentMode setting)
                                    currentMusicData
                                    model.allMusicData
                                    model.userPlayRecords
                                ]

                            -- 右側
                            , div
                                [ class "home_rightContents" ]
                                [ viewCenterArea currentMusicData model.userPlayRecords
                                , viewTopLeftArea currentMusicData
                                , viewTopRightArea currentMusicData model.rankingRecords user
                                , viewBottomLeftArea1 currentMusicData model.userPlayRecords
                                , viewBottomLeftArea2 currentMusicData model.userPlayRecords
                                , viewBottomRightArea currentMusicData
                                ]
                            ]
                        , div [] [ Page.viewLoaded ]
                        ]

                _ ->
                    div [ class "home_back" ] [ Page.viewLoading ]

        _ ->
            div [ class "home_back" ] [ Page.viewLoading ]


viewSettingIcon : Html Msg
viewSettingIcon =
    img [ class "homeUserSetting_settingIcon", src "./img/icon_setting.png", onClick ClickedSettingPanelShowBtn ] []


viewHelpIcon : Html Msg
viewHelpIcon =
    img [ class "homeUserSetting_helpIcon", src "./img/icon_help.png", onClick ClickedHelpPanelShowBtn ] []


viewInfoIcon : Html Msg
viewInfoIcon =
    img [ class "homeUserSetting_infoIcon", src "./img/icon_info.png", onClick ClickedInfoPanelShowBtn ] []


viewLogoutIcon : Html Msg
viewLogoutIcon =
    img [ class "homeUserSetting_logoutIcon", src "./img/icon_logout.png", onClick SignOut ] []


viewUser : User -> PictureUploadS -> UserSettingPanelS -> Html Msg
viewUser user pictureUploadS userSettingPanelS =
    let
        viewUserIconInput =
            case pictureUploadS of
                NotUploading ->
                    label
                        [ class "homeUserSetting_userIconBtnLabel" ]
                        [ input
                            [ class "homeUserSetting_userIconBtn"
                            , type_ "file"
                            , name "userPictureUrl"
                            , on "change" (Decode.map SelectedUserPicture Decode.value)
                            ]
                            []
                        , img
                            [ class "homeUserSetting_userIconBtnImage"
                            , src "./img/icon_photo.png"
                            ]
                            []
                        ]

                Uploading ->
                    span
                        [ class "homeUserSetting_userIconBtnLabel is-uploading" ]
                        [ img
                            [ class "homeUserSetting_userIconBtnImage is-uploading"
                            , src "./img/icon_loading.png"
                            ]
                            []
                        ]

                FailedUpload ->
                    span
                        [ class "homeUserSetting_userIconBtnLabel is-failed" ]
                        [ img
                            [ class "homeUserSetting_userIconBtnImage is-failed"
                            , src "./img/icon_attention_red.png"
                            ]
                            []
                        ]
    in
    div
        [ class "homeUserSetting_userInfoContents"
        , class <| UserSettingPanelS.clsStr userSettingPanelS
        ]
        [ img [ class "homeUserSetting_userIcon", src (User.toPictureUrl user) ] []
        , viewUserIconInput
        , input [ class "homeUserSetting_userNameInput", value (User.toName user), onInput InputUserName ] []
        ]


viewUserSettingPanel : Setting -> UserSettingPanelS -> Html Msg
viewUserSettingPanel setting userSettingPanelS =
    div [ class "homeUserSetting_userSettingPanelContents" ]
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
            , viewHelp
                |> viewIf (UserSettingPanelS.isHelp userSettingPanelS)
            , viewInfo
                |> viewIf (UserSettingPanelS.isInformation userSettingPanelS)
            ]
        ]


viewSetting : Setting -> Html Msg
viewSetting setting =
    div [ class "homeUserSettingPanel" ]
        [ div [ class "homeUserSettingPanel_title" ] [ text "Setting" ]
        , viewRangeSlider "ノーツ速度" (Setting.toNotesSpeed setting) 0.2 0.8 ChangeNotesSpeed
        , viewRangeSlider "BGM音量" (Setting.toBgmVolume setting) 0 1 ChangeBgmVolume
        , viewRangeSlider "システム音量" (Setting.toSeVolume setting) 0 1 ChangeSeVolume
        ]


viewRangeSlider : String -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
viewRangeSlider labelText value_ min max msg =
    div []
        [ div [ class "homeUserSetting_rangeSliderLabel" ] [ text labelText ]
        , input
            [ class "homeUserSetting_rangeSlider"
            , type_ "range"
            , Html.Attributes.min <| String.fromFloat min
            , Html.Attributes.max <| String.fromFloat max
            , step "0.1"
            , value <| String.fromFloat value_
            , sliderStyle value_ min max
            , onInput msg
            ]
            []
        ]


sliderStyle : Float -> Float -> Float -> Html.Attribute msg
sliderStyle value min max =
    let
        rate =
            (value - min) / (max - min)

        percentStr =
            String.fromFloat (rate * 100) ++ "%"
    in
    Html.Attributes.style
        "background-image"
        ("-webkit-gradient(linear, left top, right top, color-stop(" ++ percentStr ++ ", white), color-stop(" ++ percentStr ++ ", #8f95a3))")


viewHelp : Html msg
viewHelp =
    div [ class "homeUserSettingPanel" ]
        [ div [ class "homeUserSettingPanel_title" ] [ text "Help" ]
        , div [ class "homeUserSetting_helpText" ] [ text "WIP: 遊び方" ]
        ]


viewInfo : Html msg
viewInfo =
    div [ class "homeUserSettingPanel" ]
        [ div [ class "homeUserSettingPanel_title" ] [ text "Information" ]
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
                isSelecting =
                    mode == Setting.toCurrentMode setting
            in
            div
                [ class "homeModeSelectTab_item"
                , classIf isSelecting "is-selecting"
                , onClick <| ChangeMode mode
                ]
                [ text <| Mode.toString mode ]
    in
    div [ class "homeModeSelectTab_container" ] (List.map viewModeTabBtn allModeList)


viewMusicList : Mode -> MusicData -> AllMusicData -> UserPlayRecords -> Html Msg
viewMusicList currentMode currentMusicData allMusicData userPlayRecords =
    let
        filteredMusicData =
            AllMusicData.filterByMode currentMode allMusicData
    in
    div [ class "homeMusicList_container" ]
        (filteredMusicData
            |> List.map
                (\musicData ->
                    userPlayRecords
                        |> UserPlayRecords.findByCsvFileName musicData.csvFileName
                        |> Maybe.map (viewMusicListItem currentMusicData musicData)
                        |> Maybe.withDefault (text "")
                )
        )


viewMusicListItem : MusicData -> MusicData -> UserPlayRecord -> Html Msg
viewMusicListItem currentMusicData musicData userPlayRecord =
    let
        isSelecting =
            musicData.musicId == currentMusicData.musicId

        comboRank =
            userPlayRecord
                |> UserPlayRecord.toBestCombo
                |> Maybe.map (\combo -> Rank.newComboRank combo musicData.maxCombo)
                |> Maybe.withDefault Rank.invalid

        scoreRank =
            userPlayRecord
                |> UserPlayRecord.toBestScore
                |> Maybe.map (\score -> Rank.newScoreRank score (MusicData.toMaxScore currentMusicData))
                |> Maybe.withDefault Rank.invalid
    in
    div [ class "homeMusicListItem_container", onClick <| ChangeMusicId musicData.musicId ]
        [ div
            [ class "homeMusicListItem_topContainer" ]
            [ div
                [ class "homeMusicListItem_top", classIf isSelecting "is-selecting" ]
                [ div [ class "homeMusicListItem_topText" ] [ text musicData.musicName ] ]
            , div [ class "homeMusicListItem_topTail", classIf isSelecting "is-selecting" ] []
            , div
                [ class "homeMusicListItem_rankBox score" ]
                [ div [ class "homeMusicListItem_rankBoxBack score" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "SCORE" ]
                , div [ class "homeMusicListItem_rankText" ] [ text <| Rank.toString scoreRank ]
                ]
            , div
                [ class "homeMusicListItem_rankBox combo" ]
                [ div [ class "homeMusicListItem_rankBoxBack combo" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "COMBO" ]
                , div [ class "homeMusicListItem_rankText" ] [ text <| Rank.toString comboRank ]
                ]
            ]
        , div
            [ class "homeMusicListItem_bottomContainer" ]
            [ div [ class "homeMusicListItem_bottomText" ] [ text <| Level.toString musicData.level ]
            , div [ class "homeMusicListItem_bottomLine" ] [ div [ class "homeMusicListItem_bottomLineTail" ] [] ]
            ]
        ]


viewCenterArea : MusicData -> UserPlayRecords -> Html msg
viewCenterArea currentMusicData userPlayRecords =
    let
        playCount =
            userPlayRecords
                |> UserPlayRecords.findByCsvFileName currentMusicData.csvFileName
                |> Maybe.map UserPlayRecord.toPlayCount
                |> Maybe.withDefault 0
    in
    div [ class "home_centerArea", id "home_centerArea" ]
        [ div [ class "homeCenterArea_Inner" ] []
        , div
            [ class "homeCenterArea_ContentsContainer" ]
            [ div [ class "homeCenterArea_centerWideLine" ] []
            , div [ class "homeCenterArea_centerWideLineLeft" ] []
            , div [ class "homeCenterArea_centerWideLineRight" ] []
            , div [ class "homeCenterArea_musicName" ] [ text currentMusicData.musicName ]
            , div [ class "homeCenterArea_composer" ] [ text currentMusicData.composer ]
            , div [ class "homeCenterArea_levelText" ] [ text <| Level.toString currentMusicData.level ]
            , div [ class "homeCenterArea_box left" ] []
            , div [ class "homeCenterArea_boxLabel left" ] [ text "BPM" ]
            , div [ class "homeCenterArea_boxText left" ] [ text <| String.fromInt currentMusicData.bpm ]
            , div [ class "homeCenterArea_box center" ] []
            , div [ class "homeCenterArea_boxLabel center" ] [ text "曲の長さ" ]
            , div [ class "homeCenterArea_boxText center" ] [ text <| MusicData.toStringTime currentMusicData.fullTime ]
            , div [ class "homeCenterArea_box right" ] []
            , div [ class "homeCenterArea_boxLabel right" ] [ text "プレイ回数" ]
            , div [ class "homeCenterArea_boxText right" ] [ text <| String.fromInt playCount ++ "回" ]
            , div
                [ class "homeCenterArea_rankText title" ]
                [ div [ class "homeCenterArea_rankTitleText score" ] [ text "SCORE" ]
                , div [ class "homeCenterArea_rankTitleText combo" ] [ text "COMBO" ]
                ]
            , div [] (List.map (viewCenterAreaRankDetail currentMusicData) Rank.allRankList)
            , div [ class "homeCenterArea_rankCenterLine1" ] []
            , div [ class "homeCenterArea_rankCenterLine2" ] []
            ]
        ]


viewCenterAreaRankDetail : MusicData -> Rank -> Html msg
viewCenterAreaRankDetail currentMusicData rank =
    let
        clsRankNum =
            "rank-" ++ Rank.toString rank
    in
    div [ class "homeCenterArea_rankText", class clsRankNum ]
        [ text <| Rank.toString rank
        , div
            [ class "homeCenterArea_rankDetailText score" ]
            [ text <| String.fromInt (Rank.scoreBorder (MusicData.toMaxScore currentMusicData) rank) ]
        , div
            [ class "homeCenterArea_rankDetailText combo" ]
            [ text <| String.fromInt (Rank.comboBorder currentMusicData.maxCombo rank) ]
        , div [ class "homeCenterArea_rankLine", class clsRankNum ] []
        ]


viewTopLeftArea : MusicData -> Html msg
viewTopLeftArea currentMusicData =
    div [ class "home_topLeftArea", id "home_topLeftArea" ]
        [ div
            [ class "homeTopLeft_modeText" ]
            [ text <| Mode.toString currentMusicData.mode ]
        ]


viewTopRightArea : MusicData -> RankingRecords -> User -> Html msg
viewTopRightArea currentMusicData rankingRecords user =
    let
        maybeRankingRecord =
            RankingRecords.findByCsvFileName currentMusicData.csvFileName rankingRecords

        viewRankingItem clsRank maybeRecord =
            let
                viewRankIcon =
                    img
                        [ class "homeTopRight_rankIcon"
                        , class clsRank
                        , src <| "./img/icon_rank_" ++ clsRank ++ ".png"
                        ]
                        []
            in
            case maybeRecord of
                Just record ->
                    [ viewRankIcon
                    , img
                        [ class "homeTopRight_rankIcon user"
                        , class clsRank
                        , src <| User.toPictureUrl record.user
                        ]
                        []
                    , div
                        [ class "homeTopRight_userNameText"
                        , class clsRank
                        , classIf (RankingRecord.isOwnRecord record (User.toUid user)) "is-me"
                        ]
                        [ text <| User.toName record.user ]
                    , div
                        [ class "homeTopRight_scoreText", class clsRank ]
                        [ text <| String.fromInt record.score ]
                    , div [ class "homeTopRight_line", class clsRank ] []
                    ]

                Nothing ->
                    [ viewRankIcon
                    , div [ class "homeTopRight_noDataText", class clsRank ] [ text "No Data" ]
                    , div [ class "homeTopRight_line", class clsRank ] []
                    ]
    in
    case maybeRankingRecord of
        Just rankingRecord ->
            div [ class "home_topRightArea", id "home_topRightArea" ]
                (div [ class "homeTopRight_title" ] [ text "楽曲スコアランキング" ]
                    :: viewRankingItem "first" (RankingRecord.toFirst rankingRecord)
                    ++ viewRankingItem "second" (RankingRecord.toSecond rankingRecord)
                    ++ viewRankingItem "third" (RankingRecord.toThird rankingRecord)
                )

        Nothing ->
            text ""


viewBottomLeftArea1 : MusicData -> UserPlayRecords -> Html msg
viewBottomLeftArea1 currentMusicData userPlayRecords =
    let
        maybeBestScore =
            userPlayRecords
                |> UserPlayRecords.findByCsvFileName currentMusicData.csvFileName
                |> Maybe.andThen UserPlayRecord.toBestScore
    in
    case maybeBestScore of
        Just bestScore ->
            let
                rank =
                    Rank.newScoreRank bestScore (MusicData.toMaxScore currentMusicData)
            in
            div [ class "home_bottomLeftArea1", id "home_bottomLeftArea1" ]
                [ div [ class "homeBottomLeftArea1_label" ] [ text "SCORE" ]
                , div [ class "homeBottomLeftArea1_rankText" ] [ text <| Rank.toString rank ]
                , div [ class "homeBottomLeftArea1_bestText" ] [ text <| String.fromInt bestScore ]
                ]

        Nothing ->
            div [ class "home_bottomLeftArea1", id "home_bottomLeftArea1" ]
                [ div [ class "homeBottomLeftArea1_label" ] [ text "SCORE" ]
                , div [ class "homeBottomLeftArea1_noDataText" ] [ text "NO DATA" ]
                ]


viewBottomLeftArea2 : MusicData -> UserPlayRecords -> Html msg
viewBottomLeftArea2 currentMusicData userPlayRecords =
    let
        maybeBestCombo =
            userPlayRecords
                |> UserPlayRecords.findByCsvFileName currentMusicData.csvFileName
                |> Maybe.andThen UserPlayRecord.toBestCombo
    in
    case maybeBestCombo of
        Just bestCombo ->
            let
                rank =
                    Rank.newComboRank bestCombo currentMusicData.maxCombo
            in
            div [ class "home_bottomLeftArea2", id "home_bottomLeftArea2" ]
                [ div [ class "homeBottomLeftArea2_label" ] [ text "COMBO" ]
                , div [ class "homeBottomLeftArea2_rankText" ] [ text <| Rank.toString rank ]
                , div [ class "homeBottomLeftArea2_bestText" ] [ text <| String.fromInt bestCombo ]
                , div [ class "homeBottomLeftArea2_maxText" ] [ text <| "/ " ++ String.fromInt currentMusicData.maxCombo ]
                ]

        Nothing ->
            div [ class "home_bottomLeftArea2", id "home_bottomLeftArea2" ]
                [ div [ class "homeBottomLeftArea2_label" ] [ text "COMBO" ]
                , div [ class "homeBottomLeftArea2_noDataText" ] [ text "NO DATA" ]
                ]


viewBottomRightArea : MusicData -> Html Msg
viewBottomRightArea currentMusicData =
    div []
        -- 戻るボタンでHomeに戻ることを許容する
        [ a
            [ Route.href <| Route.Play (Just <| currentMusicData.csvFileName)
            , onMouseUp PlayPlayBtnSE
            ]
            [ div
                [ class "home_bottomRightArea", id "home_bottomRightArea" ]
                [ div [ class "homeBottomRight_playText" ] [ text "Play" ] ]
            ]
        , div [ class "homeBottomRight_transparentCover1" ] []
        , div [ class "homeBottomRight_transparentCover2" ] []
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toAllMusicData : Model -> AllMusicData
toAllMusicData model =
    model.allMusicData


toAudioLoadingS : Model -> AudioLoadingS
toAudioLoadingS model =
    model.audioLoadingS


toUserSetting : Model -> UserSetting
toUserSetting model =
    model.userSetting
