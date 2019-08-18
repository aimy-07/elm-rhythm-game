module Page.Play.JudgeEffect exposing (JudgeEffect, new)

import Page.Play.JudgeKind as JudgeKind exposing (JudgeKind)
import Page.Play.LinePosition as LinePosition exposing (LinePosition)


type alias JudgeEffect =
    { styleLeft : String
    , judgeText : String
    }


new : JudgeKind -> LinePosition -> JudgeEffect
new judgeKind position =
    { styleLeft = LinePosition.styleLeft position
    , judgeText = JudgeKind.toStringJudgeKind judgeKind
    }
