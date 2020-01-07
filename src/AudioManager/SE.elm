module AudioManager.SE exposing (SE(..), seKeyList, unwrap)


type SE
    = Select
    | Cancel
    | Decision
    | Cursor
    | Succeed
    | SelectPlayMusic
    | Countdown
    | Result
    | Attention


unwrap : SE -> String
unwrap se =
    case se of
        Select ->
            "select"

        Cancel ->
            "cancel"

        Decision ->
            "decision"

        Cursor ->
            "cursor"

        Succeed ->
            "succeed"

        SelectPlayMusic ->
            "selectPlayMusic"

        Countdown ->
            "countdown"

        Result ->
            "result"

        Attention ->
            "attention"


seKeyList : List String
seKeyList =
    [ Select
    , Cancel
    , Decision
    , Cursor
    , Succeed
    , SelectPlayMusic
    , Countdown
    , Result
    , Attention
    ]
        |> List.map unwrap
