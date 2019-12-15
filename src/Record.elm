module Record exposing (Record, RecordDto, new)

import User exposing (Uid)


type alias Record =
    { uid : Uid
    , combo : Int
    , score : Int
    }


type alias RecordDto =
    { uid : String
    , combo : Int
    , score : Int
    }


new : RecordDto -> Record
new { uid, combo, score } =
    { uid = uid
    , combo = combo
    , score = score
    }
