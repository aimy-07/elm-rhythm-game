port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import AllMusicInfoList exposing (AllMusicInfoList)
import Constants exposing (allMode, notesSpeedLevel)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.CsvFileName as CsvFileName
import MusicInfo.Level as Level
import MusicInfo.Mode as Mode exposing (Mode)
import MusicInfo.MusicId exposing (MusicId)
import OwnRecord exposing (OwnRecord, OwnRecordDto)
import Page
import PublicRecord exposing (PublicRecord, PublicRecordDto)
import Rank exposing (Rank)
import Route
import Session exposing (Session)
import User exposing (User)
import UserSetting exposing (UserSettingData, UserSettingDto)
import UserSetting.NotesSpeed exposing (NotesSpeed)
import Utils exposing (cmdIf, viewIf)



-- MODEL


type alias Model =
    { session : Session
    , maybeOwnRecords : Maybe (List OwnRecord)
    , maybePublicRecords : Maybe (List PublicRecord)
    , pictureUploadS : PictureUploadS
    }


type PictureUploadS
    = NotUploading
    | Uploading


initModel : Session -> Model
initModel session =
    { session = session
    , maybeOwnRecords = Nothing
    , maybePublicRecords = Nothing
    , pictureUploadS = NotUploading
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        allMusicInfoList =
            Session.toAllMusicInfoList session
    in
    case Session.toUser session of
        Just user ->
            let
                updatedSession =
                    Session.resetUserSetting session
            in
            ( initModel updatedSession
            , Cmd.batch
                [ getAllMusicInfoList ()
                    |> cmdIf (not <| AllMusicInfoList.isLoaded allMusicInfoList)
                , getUserSetting user.uid
                , getOwnRecords user.uid
                , getPublicRecords ()
                , startHomeMusic ()
                ]
            )

        Nothing ->
            -- Homeで user == Nothing にはまずならないが、念のためログイン画面に戻す処理を入れておく
            ( initModel <| Session.init (Session.toNavKey session)
            , Route.replaceUrl (Session.toNavKey session) Route.Login
            )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)
    | GotUserSetting UserSettingDto
    | GotOwnRecords (List OwnRecordDto)
    | GotPublicRecords (List PublicRecordDto)
    | ChangeMusicId MusicId
    | ChangeMode Mode
    | ChangeNotesSpeed NotesSpeed
    | InputUserName String
    | SelectdPicture Decode.Value
    | SavedUserPicture String
    | SignOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Session.toUser model.session of
        Just user ->
            case msg of
                GotAllMusicInfoList musicInfoDtos ->
                    let
                        session =
                            Session.setAllMusicInfoList musicInfoDtos model.session
                    in
                    ( { model | session = session }, Cmd.none )

                GotUserSetting userSettingDto ->
                    let
                        session =
                            Session.setUserSetting userSettingDto model.session
                    in
                    ( { model | session = session }, Cmd.none )

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

                ChangeMusicId musicId ->
                    let
                        updatedSession =
                            Session.updateUserSetting musicId UserSetting.updateCurrentMusicId model.session
                    in
                    ( { model | session = updatedSession }
                    , Cmd.batch
                        [ saveCurrentMusicId { uid = user.uid, currentMusicId = musicId }
                        , playMusicSelectAnim ()
                        ]
                    )

                ChangeMode mode ->
                    let
                        updatedSession =
                            Session.updateUserSetting mode UserSetting.updateCurrentMode model.session
                    in
                    ( { model | session = updatedSession }
                    , Cmd.batch
                        [ saveCurrentMode { uid = user.uid, currentMode = Mode.unwrap mode }
                        , playMusicSelectAnim ()
                        ]
                    )

                ChangeNotesSpeed notesSpeed ->
                    let
                        updatedSession =
                            Session.updateUserSetting notesSpeed UserSetting.updateNotesSpeed model.session
                    in
                    ( { model | session = updatedSession }
                    , saveNotesSpeed { uid = user.uid, notesSpeed = notesSpeed }
                    )

                InputUserName nextUserName ->
                    let
                        updatedSession =
                            Session.updateUser nextUserName User.updateUserName model.session
                    in
                    ( { model | session = updatedSession }
                    , saveUserName { uid = user.uid, userName = nextUserName }
                    )

                SelectdPicture event ->
                    ( { model | pictureUploadS = Uploading }
                    , saveUserPicture { uid = user.uid, event = event }
                    )

                SavedUserPicture url ->
                    let
                        updatedSession =
                            Session.updateUser url User.updatePictureUrl model.session
                    in
                    ( { model | pictureUploadS = NotUploading, session = updatedSession }, Cmd.none )

                SignOut ->
                    ( model, signOut () )

        Nothing ->
            -- Homeで user == Nothing にはまずならないが、念のためログイン画面に戻す処理を入れておく
            ( { model | session = Session.init (Session.toNavKey model.session) }
            , Route.replaceUrl (Session.toNavKey model.session) Route.Login
            )



-- PORT


port getAllMusicInfoList : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg


port getUserSetting : String -> Cmd msg


port gotUserSetting : (UserSettingDto -> msg) -> Sub msg


port saveCurrentMusicId : { uid : String, currentMusicId : String } -> Cmd msg


port saveCurrentMode : { uid : String, currentMode : String } -> Cmd msg


port saveNotesSpeed : { uid : String, notesSpeed : Float } -> Cmd msg


port getOwnRecords : String -> Cmd msg


port gotOwnRecords : (List OwnRecordDto -> msg) -> Sub msg


port getPublicRecords : () -> Cmd msg


port gotPublicRecords : (List PublicRecordDto -> msg) -> Sub msg


port saveUserName : { uid : String, userName : String } -> Cmd msg


port saveUserPicture : { uid : String, event : Decode.Value } -> Cmd msg


port savedUserPicture : (String -> msg) -> Sub msg


port playMusicSelectAnim : () -> Cmd msg


port startHomeMusic : () -> Cmd msg


port signOut : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotAllMusicInfoList GotAllMusicInfoList
        , gotUserSetting GotUserSetting
        , gotOwnRecords GotOwnRecords
        , gotPublicRecords GotPublicRecords
        , savedUserPicture SavedUserPicture
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

        maybeUserSetting =
            Session.toUserSetting model.session
                |> UserSetting.toMaybe

        allMusicInfoList =
            Session.toAllMusicInfoList model.session

        currentCsvFileName =
            maybeUserSetting
                |> Maybe.map (\setting -> CsvFileName.new setting.currentMusicId setting.currentMode)
                |> Maybe.withDefault ""

        maybeCurrentMusicInfo =
            AllMusicInfoList.findByCsvFileName currentCsvFileName allMusicInfoList
    in
    case ( maybeUser, maybeUserSetting ) of
        ( Just user, Just userSetting ) ->
            case ( maybeCurrentMusicInfo, model.maybeOwnRecords, model.maybePublicRecords ) of
                ( Just currentMusicInfo, Just ownRecords, Just publicRecords ) ->
                    let
                        maybeCurrentOwnRecord =
                            OwnRecord.findByCsvFileName currentMusicInfo.csvFileName ownRecords

                        maybeCurrentPublicRecord =
                            PublicRecord.findByCsvFileName currentMusicInfo.csvFileName publicRecords
                    in
                    div [ class "home_contentsContainer" ]
                        -- 左側
                        [ div
                            [ class "home_leftContentsContainer" ]
                            [ div
                                [ class "home_leftContents" ]
                                [ div
                                    [ class "homeUserSetting_container" ]
                                    [ viewUser user model.pictureUploadS
                                    , viewSetting userSetting
                                    ]
                                , viewModeTab userSetting
                                , viewMusicList userSetting.currentMode currentMusicInfo allMusicInfoList ownRecords
                                ]
                            ]

                        -- 右側
                        , div
                            [ class "home_rightContentsContainer" ]
                            [ div
                                [ class "home_rightContents" ]
                                [ viewCenterArea currentMusicInfo maybeCurrentOwnRecord
                                , viewTopLeftArea currentMusicInfo
                                , viewTopRightArea maybeCurrentPublicRecord
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


viewUser : User -> PictureUploadS -> Html Msg
viewUser user pictureUploadS =
    let
        clsIsUploading =
            if pictureUploadS == Uploading then
                "is-uploading"

            else
                ""

        isUploading =
            pictureUploadS == Uploading
    in
    div
        [ class "homeUserSetting_leftContents" ]
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
        , img [ class "homeUserSetting_logoutIcon", src "./img/icon_logout.png", onClick SignOut ] []
        ]


viewSetting : UserSettingData -> Html Msg
viewSetting userSetting =
    let
        viewNotesSpeed notesSpeed =
            if notesSpeed <= userSetting.notesSpeed then
                span [ class "homeUserSetting_settingBtn", onClick <| ChangeNotesSpeed notesSpeed ] [ text "◆" ]

            else
                span [ class "homeUserSetting_settingBtn", onClick <| ChangeNotesSpeed notesSpeed ] [ text "◇" ]
    in
    div
        [ class "homeUserSetting_rightContents" ]
        [ div
            [ class "homeUserSetting_settingContainer" ]
            [ div
                [ class "homeUserSetting_settingItem" ]
                [ text "ノーツ速度"
                , div [ class "homeUserSetting_settingBtnContainer" ] (List.map viewNotesSpeed notesSpeedLevel)
                ]
            , viewSettingItem "BGM Volume"
            , viewSettingItem "タップ音 Volume"
            , viewSettingItem "システム音 Volume"
            ]
        ]


viewSettingItem : String -> Html Msg
viewSettingItem labelText =
    -- TODO: 将来的には不要になる
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


viewModeTab : UserSettingData -> Html Msg
viewModeTab userSetting =
    let
        viewModeTabBtn mode =
            let
                clsIsSelecting =
                    if mode == userSetting.currentMode then
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


viewTopRightArea : Maybe PublicRecord -> Html msg
viewTopRightArea maybeCurrentPublicRecord =
    div [ class "home_topRightArea", id "home_topRightArea" ]
        [ div [ class "homeTopRight_title" ] [ text "楽曲スコアランキング" ]
        , img [ class "homeTopRight_rankIcon first", src "./img/icon_rank_first.png" ] []
        , div
            [ class "homeTopRight_userNameText first" ]
            [ text <| PublicRecord.toStringUserName (PublicRecord.toFirstScoreRecord maybeCurrentPublicRecord) ]
        , div
            [ class "homeTopRight_scoreText first" ]
            [ text <| PublicRecord.toStringScore (PublicRecord.toFirstScoreRecord maybeCurrentPublicRecord) ]
        , div [ class "homeTopRight_line first" ] []
        , img [ class "homeTopRight_rankIcon second", src "./img/icon_rank_second.png" ] []
        , div
            [ class "homeTopRight_userNameText second" ]
            [ text <| PublicRecord.toStringUserName (PublicRecord.toSecondScoreRecord maybeCurrentPublicRecord) ]
        , div
            [ class "homeTopRight_scoreText second" ]
            [ text <| PublicRecord.toStringScore (PublicRecord.toSecondScoreRecord maybeCurrentPublicRecord) ]
        , div [ class "homeTopRight_line second" ] []
        , img [ class "homeTopRight_rankIcon third", src "./img/icon_rank_third.png" ] []
        , div
            [ class "homeTopRight_userNameText third" ]
            [ text <| PublicRecord.toStringUserName (PublicRecord.toThirdScoreRecord maybeCurrentPublicRecord) ]
        , div
            [ class "homeTopRight_scoreText third" ]
            [ text <| PublicRecord.toStringScore (PublicRecord.toThirdScoreRecord maybeCurrentPublicRecord) ]
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
        -- 戻るボタンでHomeに戻ることを許容する
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
