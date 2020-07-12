module Page.Home.UserSettingPanelS exposing
    ( UserSettingPanelS
    , close
    , clsStr
    , init
    , isHelp
    , isInformation
    , isSetting
    , showHelp
    , showInformation
    , showSetting
    )


type UserSettingPanelS
    = Init
    | Show PanelKind
    | Hide PanelKind


type PanelKind
    = Setting
    | Help
    | Information


init : UserSettingPanelS
init =
    Init


showSetting : UserSettingPanelS
showSetting =
    Show Setting


showHelp : UserSettingPanelS
showHelp =
    Show Help


showInformation : UserSettingPanelS
showInformation =
    Show Information


close : UserSettingPanelS -> UserSettingPanelS
close userSettingPanelS =
    case userSettingPanelS of
        Show kind ->
            Hide kind

        Hide _ ->
            Init

        Init ->
            Init


isSetting : UserSettingPanelS -> Bool
isSetting userSettingPanelS =
    toKind userSettingPanelS
        |> Maybe.map ((==) Setting)
        |> Maybe.withDefault False


isHelp : UserSettingPanelS -> Bool
isHelp userSettingPanelS =
    toKind userSettingPanelS
        |> Maybe.map ((==) Help)
        |> Maybe.withDefault False


isInformation : UserSettingPanelS -> Bool
isInformation userSettingPanelS =
    toKind userSettingPanelS
        |> Maybe.map ((==) Information)
        |> Maybe.withDefault False


toKind : UserSettingPanelS -> Maybe PanelKind
toKind userSettingPanelS =
    case userSettingPanelS of
        Show kind ->
            Just kind

        Hide kind ->
            Just kind

        Init ->
            Nothing


clsStr : UserSettingPanelS -> String
clsStr userSettingPanelS =
    case userSettingPanelS of
        Show _ ->
            "is-open"

        Hide _ ->
            "is-close"

        Init ->
            ""
