module User.AccountType exposing (AccountType(..), toString)


type AccountType
    = Google
    | Twitter
    | Github


toString : AccountType -> String
toString accountType =
    case accountType of
        Google ->
            "Google"

        Twitter ->
            "Twitter"

        Github ->
            "Github"
