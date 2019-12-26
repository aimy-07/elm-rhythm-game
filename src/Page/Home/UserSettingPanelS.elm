module Page.Home.UserSettingPanelS exposing
    ( UserSettingPanelS(..)
    , close
    , clsStr
    , init
    , isInfo
    , isSetting
    )


type UserSettingPanelS
    = Init
    | SettingShow
    | SettingHide
    | InfoShow
    | InfoHide


init : UserSettingPanelS
init =
    Init


close : UserSettingPanelS -> UserSettingPanelS
close userSettingPanelS =
    case userSettingPanelS of
        SettingShow ->
            SettingHide

        SettingHide ->
            Init

        InfoShow ->
            InfoHide

        InfoHide ->
            Init

        Init ->
            Init


clsStr : UserSettingPanelS -> String
clsStr userSettingPanelS =
    case userSettingPanelS of
        SettingShow ->
            "is-open"

        InfoShow ->
            "is-open"

        SettingHide ->
            "is-close"

        InfoHide ->
            "is-close"

        Init ->
            ""


isSetting : UserSettingPanelS -> Bool
isSetting userSettingPanelS =
    userSettingPanelS == SettingShow || userSettingPanelS == SettingHide


isInfo : UserSettingPanelS -> Bool
isInfo userSettingPanelS =
    userSettingPanelS == InfoShow || userSettingPanelS == InfoHide
