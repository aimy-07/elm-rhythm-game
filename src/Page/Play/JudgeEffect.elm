module Page.Play.JudgeEffect exposing (JudgeEffect, new)

import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)


type alias JudgeEffect =
    { styleLeft : String
    , judgeText : String
    , noteType : String
    }


new : JudgeKind -> LinePosition -> Bool -> JudgeEffect
new judgeKind position isLong =
    { styleLeft = LinePosition.styleLeft position
    , judgeText = JudgeKind.toStringJudgeKind judgeKind
    , noteType =
        if isLong then
            "LONG"

        else
            "SINGLE"
    }
