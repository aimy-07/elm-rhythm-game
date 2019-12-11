port module Page.Play.Judge exposing
    ( Judge(..)
    , isOverJustTime
    , isOverMiss
    , judgeKeyDown
    , keyDownEffectCmd
    , longEffectCmd
    , missEffectCmd
    , toStringJudge
    )

import Constants exposing (goodRange, greatRange, missRange, perfectRange)
import Page
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.KeyStr exposing (KeyStr)
import Page.Play.Note.JustTime exposing (JustTime)


type Judge
    = Perfect
    | Great
    | Good
    | Miss
    | Invalid


toStringJudge : Judge -> String
toStringJudge judge =
    case judge of
        Perfect ->
            "Perfect"

        Great ->
            "Great"

        Good ->
            "Good"

        Miss ->
            "Miss"

        Invalid ->
            ""


judgeKeyDown : CurrentMusicTime -> JustTime -> Judge
judgeKeyDown currentMusicTime justTime =
    if Basics.abs (justTime - currentMusicTime) < perfectRange then
        Perfect

    else if Basics.abs (justTime - currentMusicTime) < greatRange then
        Great

    else if Basics.abs (justTime - currentMusicTime) < goodRange then
        Good

    else if justTime - currentMusicTime < missRange then
        Miss

    else
        Invalid


isOverMiss : CurrentMusicTime -> JustTime -> Bool
isOverMiss currentMusicTime justTime =
    justTime + goodRange < currentMusicTime


isOverJustTime : CurrentMusicTime -> JustTime -> Bool
isOverJustTime currentMusicTime justTime =
    justTime < currentMusicTime


keyDownEffectCmd : KeyStr -> Judge -> Bool -> Cmd msg
keyDownEffectCmd keyStr judge isLongNote =
    Cmd.batch
        [ playJudgeEffectAnim
            { keyStr = keyStr
            , isLongNote = isLongNote
            }
        , playJudgeEffectTextAnim
            { keyStr = keyStr
            , judgeText = toStringJudge judge
            }
        ]
        |> Page.cmdIf (judge /= Invalid)


missEffectCmd : KeyStr -> Cmd msg
missEffectCmd keyStr =
    playJudgeEffectTextAnim
        { keyStr = keyStr
        , judgeText = toStringJudge Miss
        }


longEffectCmd : KeyStr -> Cmd msg
longEffectCmd keyStr =
    playJudgeEffectAnim
        { keyStr = keyStr
        , isLongNote = True
        }


port playJudgeEffectAnim : { keyStr : String, isLongNote : Bool } -> Cmd msg


port playJudgeEffectTextAnim : { keyStr : String, judgeText : String } -> Cmd msg
