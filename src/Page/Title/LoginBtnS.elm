module Page.Title.LoginBtnS exposing
    ( LoginBtnS
    , init
    , isDisabled
    , isSelecting
    , isShow
    , toDisabled
    , toShow
    )

import Session.User.AccountType exposing (AccountType(..))


type LoginBtnS
    = ShowDisabled AccountType
    | Show
    | Hide


init : LoginBtnS
init =
    Hide


toShow : LoginBtnS -> LoginBtnS
toShow loginBtnS =
    case loginBtnS of
        Hide ->
            Show

        ShowDisabled _ ->
            Show

        Show ->
            loginBtnS


toDisabled : AccountType -> LoginBtnS -> LoginBtnS
toDisabled accountType loginBtnS =
    case loginBtnS of
        Show ->
            ShowDisabled accountType

        ShowDisabled _ ->
            loginBtnS

        Hide ->
            loginBtnS


isShow : LoginBtnS -> Bool
isShow loginBtnS =
    case loginBtnS of
        ShowDisabled _ ->
            True

        Show ->
            True

        Hide ->
            False


isDisabled : LoginBtnS -> Bool
isDisabled loginBtnS =
    case loginBtnS of
        ShowDisabled _ ->
            True

        Show ->
            False

        Hide ->
            False


isSelecting : AccountType -> LoginBtnS -> Bool
isSelecting accountType loginBtnS =
    case loginBtnS of
        ShowDisabled accountType_ ->
            accountType_ == accountType

        Show ->
            False

        Hide ->
            False
