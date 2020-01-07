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
import Html exposing (Html, a, div, img, input, label, text)
import Html.Attributes exposing (class, disabled, href, id, name, src, step, target, type_, value)
import Html.Events exposing (on, onClick, onInput, onMouseUp)
import Json.Decode as Decode
import OwnRecord exposing (OwnRecordDto)
import Page
import Page.Home.RankingRecords as RankingRecords exposing (RankingData, RankingRecords)
import Page.Home.UserPlayRecords as UserPlayRecords exposing (UserPlayRecordData, UserPlayRecords)
import Page.Home.UserSettingPanelS as UserSettingPanelS exposing (UserSettingPanelS(..))
import PublicRecord exposing (PublicRecordDto)
import Rank exposing (Rank)
import Route
import Session exposing (Session)
import Session.User as User exposing (User, UserDto)
import UserSetting exposing (UserSetting, UserSettingDto)
import UserSetting.Setting as Setting exposing (Setting)
import UserSetting.Setting.NotesSpeed as NotesSpeed
import UserSetting.Setting.Volume as Volume
import Utils exposing (viewIf)



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
                [ UserPlayRecords.initCmd user.uid
                , RankingRecords.initCmd
                , UserSetting.getUserSetting user.uid
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
    | SelectdPicture Decode.Value
    | SavedUserPicture String
    | ClickedSettingPanelShowBtn
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
                        |> Maybe.map .bgmVolume

                seVolume =
                    UserSetting.toSetting model.userSetting
                        |> Maybe.map .seVolume
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
                                |> Maybe.map .currentMusicId
                                |> Maybe.withDefault currentMusicIdDefault

                        updatedBgmVolume =
                            UserSetting.toSetting userSetting
                                |> Maybe.map .bgmVolume
                    in
                    ( { model | userSetting = userSetting }
                    , AudioManager.playBGM (BGM.sampleFromMusicId currentMusicId) updatedBgmVolume
                    )

                ChangeMusicId musicId ->
                    let
                        updatedUserSetting =
                            UserSetting.updateCurrentMusicId musicId model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveCurrentMusicId { uid = user.uid, currentMusicId = musicId }
                        , AudioManager.playBGM (BGM.sampleFromMusicId musicId) bgmVolume
                        , AudioManager.playSE SE.SelectPlayMusic seVolume
                        , AnimationManager.playMusicSelectAnim ()
                        ]
                    )

                ChangeMode mode ->
                    let
                        updatedUserSetting =
                            UserSetting.updateCurrentMode mode model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveCurrentMode { uid = user.uid, currentMode = Mode.unwrap mode }
                        , AudioManager.playSE SE.SelectPlayMusic seVolume
                        , AnimationManager.playMusicSelectAnim ()
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
                    , Cmd.batch
                        [ Setting.saveNotesSpeed { uid = user.uid, notesSpeed = notesSpeed }
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                ChangeBgmVolume volumeValue ->
                    let
                        nextBgmVolume =
                            Volume.fromString volumeValue

                        updatedUserSetting =
                            UserSetting.updateBgmVolume nextBgmVolume model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveBgmVolume { uid = user.uid, bgmVolume = nextBgmVolume }
                        , AudioManager.changeBgmVolume nextBgmVolume
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                ChangeSeVolume volumeValue ->
                    let
                        nextSeVolume =
                            Volume.fromString volumeValue

                        updatedUserSetting =
                            UserSetting.updateSeVolume nextSeVolume model.userSetting
                    in
                    ( { model | userSetting = updatedUserSetting }
                    , Cmd.batch
                        [ Setting.saveSeVolume { uid = user.uid, seVolume = nextSeVolume }
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
                    )

                InputUserName nextUserName ->
                    let
                        updatedSession =
                            Session.updateUser nextUserName User.updateUserName model.session
                    in
                    ( { model | session = updatedSession }
                    , Cmd.batch
                        [ User.saveUserName { uid = user.uid, userName = nextUserName }
                        , AudioManager.playSE SE.Cursor seVolume
                        ]
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
                    ( { model | pictureUploadS = NotUploading, session = updatedSession }
                    , AudioManager.playSE SE.Succeed seVolume
                    )

                ClickedSettingPanelShowBtn ->
                    ( { model | userSettingPanelS = SettingShow }
                    , AudioManager.playSE SE.Select seVolume
                    )

                ClickedInfoPanelShowBtn ->
                    ( { model | userSettingPanelS = InfoShow }
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
        , User.savedUserPicture SavedUserPicture
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
                    CsvFileName.new setting.currentMusicId setting.currentMode

                maybeCurrentMusicData =
                    AllMusicData.findByCsvFileName currentCsvFileName model.allMusicData

                maybeUserPlayRecordData =
                    UserPlayRecords.findByCsvFileName currentCsvFileName model.userPlayRecords

                maybeRankingData =
                    RankingRecords.findByCsvFileName currentCsvFileName model.rankingRecords
            in
            case ( maybeCurrentMusicData, maybeUserPlayRecordData, maybeRankingData ) of
                ( Just currentMusicData, Just userPlayRecordData, Just rankingData ) ->
                    div
                        [ class "home_back" ]
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
                                , viewMusicList setting.currentMode currentMusicData model.allMusicData model.userPlayRecords
                                ]

                            -- 右側
                            , div
                                [ class "home_rightContents" ]
                                [ viewCenterArea currentMusicData userPlayRecordData
                                , viewTopLeftArea currentMusicData
                                , viewTopRightArea rankingData user
                                , viewBottomLeftArea1 currentMusicData userPlayRecordData
                                , viewBottomLeftArea2 currentMusicData userPlayRecordData
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
        , viewRangeSlider "ノーツ速度" setting.notesSpeed 0.2 0.8 ChangeNotesSpeed
        , viewRangeSlider "BGM音量" setting.bgmVolume 0 1 ChangeBgmVolume
        , viewRangeSlider "システム音量" setting.seVolume 0 1 ChangeSeVolume
        ]


viewRangeSlider : String -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
viewRangeSlider label value_ min max msg =
    div []
        [ div [ class "homeUserSetting_rangeSliderLabel" ] [ text label ]
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
                    let
                        maybeUserPlayRecordData =
                            UserPlayRecords.findByCsvFileName musicData.csvFileName userPlayRecords
                    in
                    viewMusicListItem currentMusicData musicData maybeUserPlayRecordData
                )
        )


viewMusicListItem : MusicData -> MusicData -> Maybe UserPlayRecordData -> Html Msg
viewMusicListItem currentMusicData musicData maybeUserPlayRecordData =
    let
        clsIsSelecting =
            if musicData.musicId == currentMusicData.musicId then
                "is-selecting"

            else
                ""

        comboRank =
            maybeUserPlayRecordData
                |> Maybe.map
                    (\record ->
                        record.bestCombo
                            |> Maybe.map (\combo -> Rank.newComboRank combo musicData.maxCombo)
                            |> Maybe.withDefault Rank.invalid
                    )
                |> Maybe.withDefault Rank.invalid

        scoreRank =
            maybeUserPlayRecordData
                |> Maybe.map
                    (\record ->
                        record.bestScore
                            |> Maybe.map (\score -> Rank.newScoreRank score musicData.maxScore)
                            |> Maybe.withDefault Rank.invalid
                    )
                |> Maybe.withDefault Rank.invalid
    in
    div
        [ class "homeMusicListItem_container"
        , onClick <| ChangeMusicId musicData.musicId
        ]
        [ div
            [ class "homeMusicListItem_topContainer" ]
            [ div
                [ class "homeMusicListItem_top", class clsIsSelecting ]
                [ div [ class "homeMusicListItem_topText" ] [ text musicData.musicName ]
                ]
            , div [ class "homeMusicListItem_topTail", class clsIsSelecting ] []
            , div
                [ class "homeMusicListItem_rankBox combo" ]
                [ div [ class "homeMusicListItem_rankBoxBack combo" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "COMBO" ]
                , div
                    [ class "homeMusicListItem_rankText" ]
                    [ text <| Rank.toString comboRank ]
                ]
            , div [ class "homeMusicListItem_rankBox score" ]
                [ div [ class "homeMusicListItem_rankBoxBack score" ] []
                , div [ class "homeMusicListItem_rankLabel" ] [ text "SCORE" ]
                , div
                    [ class "homeMusicListItem_rankText" ]
                    [ text <| Rank.toString scoreRank ]
                ]
            ]
        , div
            [ class "homeMusicListItem_bottomContainer" ]
            [ div [ class "homeMusicListItem_bottomText" ] [ text <| Level.toString musicData.level ]
            , div [ class "homeMusicListItem_bottomLine" ]
                [ div [ class "homeMusicListItem_bottomLineTail" ] []
                ]
            ]
        ]


viewCenterArea : MusicData -> UserPlayRecordData -> Html msg
viewCenterArea currentMusicData userPlayRecordData =
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
            , div [ class "homeCenterArea_boxText right" ] [ text <| String.fromInt userPlayRecordData.playCount ++ "回" ]
            , div
                [ class "homeCenterArea_rankText title" ]
                [ div [ class "homeCenterArea_rankTitleText score" ] [ text "SCORE" ]
                , div [ class "homeCenterArea_rankTitleText combo" ] [ text "COMBO" ]
                ]
            , div
                []
                (List.map (viewCenterAreaRankDetail currentMusicData) Rank.allRankList)
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
            [ text <| String.fromInt (Rank.scoreBorder currentMusicData.maxScore rank) ]
        , div
            [ class "homeCenterArea_rankDetailText combo" ]
            [ text <| String.fromInt (Rank.comboBorder currentMusicData.maxCombo rank) ]
        , div
            [ class "homeCenterArea_rankLine", class clsRankNum ]
            []
        ]


viewTopLeftArea : MusicData -> Html msg
viewTopLeftArea currentMusicData =
    div [ class "home_topLeftArea", id "home_topLeftArea" ]
        [ div [ class "homeTopLeft_modeText" ] [ text <| Mode.toString currentMusicData.mode ]
        ]


viewTopRightArea : RankingData -> User -> Html msg
viewTopRightArea rankingData user =
    let
        viewRankingItem clsRank maybeRecord =
            let
                clsIsMe =
                    if RankingRecords.isOwnRecord maybeRecord user.uid then
                        "is-Me"

                    else
                        ""

                userName =
                    maybeRecord
                        |> Maybe.map (.user >> .userName)
                        |> Maybe.withDefault "---"

                score =
                    maybeRecord
                        |> Maybe.map (.score >> String.fromInt)
                        |> Maybe.withDefault "---"

                viewUserIcon =
                    maybeRecord
                        |> Maybe.map (.user >> .pictureUrl)
                        |> Maybe.map (\url -> img [ class "homeTopRight_rankIcon user", class clsRank, src url ] [])
                        |> Maybe.withDefault (text "")

                rankIconSrc =
                    "./img/icon_rank_" ++ clsRank ++ ".png"
            in
            [ img [ class "homeTopRight_rankIcon", class clsRank, src rankIconSrc ] []
            , viewUserIcon
            , div [ class "homeTopRight_userNameText", class clsRank, class clsIsMe ] [ text userName ]
            , div [ class "homeTopRight_scoreText", class clsRank ] [ text score ]
            , div [ class "homeTopRight_line", class clsRank ] []
            ]
    in
    div
        [ class "home_topRightArea"
        , id "home_topRightArea"
        ]
        (div [ class "homeTopRight_title" ] [ text "楽曲スコアランキング" ]
            :: viewRankingItem "first" rankingData.first
            ++ viewRankingItem "second" rankingData.second
            ++ viewRankingItem "third" rankingData.third
        )


viewBottomLeftArea1 : MusicData -> UserPlayRecordData -> Html msg
viewBottomLeftArea1 currentMusicData userPlayRecordData =
    let
        comboRank =
            userPlayRecordData.bestCombo
                |> Maybe.map (\combo -> Rank.newComboRank combo currentMusicData.maxCombo)
                |> Maybe.withDefault Rank.invalid

        comboStr =
            userPlayRecordData.bestCombo
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "---"
    in
    div [ class "home_bottomLeftArea1", id "home_bottomLeftArea1" ]
        [ div [ class "homeBottomLeftArea1_label" ] [ text "COMBO" ]
        , div
            [ class "homeBottomLeftArea1_rankText" ]
            [ text <| Rank.toString comboRank ]
        , div
            [ class "homeBottomLeftArea1_bestText" ]
            [ text comboStr ]
        , div
            [ class "homeBottomLeftArea1_maxText" ]
            [ text <| "/ " ++ String.fromInt currentMusicData.maxCombo ]
        ]


viewBottomLeftArea2 : MusicData -> UserPlayRecordData -> Html msg
viewBottomLeftArea2 currentMusicData userPlayRecordData =
    let
        scoreRank =
            userPlayRecordData.bestScore
                |> Maybe.map (\score -> Rank.newScoreRank score currentMusicData.maxScore)
                |> Maybe.withDefault Rank.invalid

        scoreStr =
            userPlayRecordData.bestScore
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "---"
    in
    div [ class "home_bottomLeftArea2", id "home_bottomLeftArea2" ]
        [ div [ class "homeBottomLeftArea2_label" ] [ text "SCORE" ]
        , div
            [ class "homeBottomLeftArea2_rankText" ]
            [ text <| Rank.toString scoreRank ]
        , div
            [ class "homeBottomLeftArea2_bestText" ]
            [ text scoreStr ]
        , div
            [ class "homeBottomLeftArea2_maxText" ]
            [ text <| "/ " ++ String.fromInt currentMusicData.maxScore ]
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


toAllMusicData : Model -> AllMusicData
toAllMusicData model =
    model.allMusicData


toAudioLoadingS : Model -> AudioLoadingS
toAudioLoadingS model =
    model.audioLoadingS


toUserSetting : Model -> UserSetting
toUserSetting model =
    model.userSetting
