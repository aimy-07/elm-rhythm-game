module AudioManager.SE exposing (SE(..), seKeyList, unwrap)


type SE
    = SelectPlayMusic


unwrap : SE -> String
unwrap se =
    case se of
        SelectPlayMusic ->
            "selectPlayMusic"


seKeyList : List String
seKeyList =
    [ "selectPlayMusic" ]
