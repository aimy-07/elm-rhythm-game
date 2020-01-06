module Page.Play.Judge exposing
    ( Judge(..)
    , JudgeEffect
    , isOverJustTime
    , isOverMiss
    , judgeEffectCmd
    , judgeKeyDown
    , playMissEffectAnimCmd
    , toString
    )

import AnimationManager
import Constants exposing (goodRange, greatRange, perfectRange)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.KeyStr exposing (KeyStr)
import Page.Play.Note.JustTime exposing (JustTime)
import Utils exposing (cmdIf)


type Judge
    = Perfect
    | Great
    | Good
    | Lost
    | Miss
    | Invalid


toString : Judge -> String
toString judge =
    case judge of
        Perfect ->
            "Perfect"

        Great ->
            "Great"

        Good ->
            "Good"

        Lost ->
            "Lost"

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

    else
        Invalid


isOverMiss : CurrentMusicTime -> JustTime -> Bool
isOverMiss currentMusicTime justTime =
    justTime + goodRange < currentMusicTime


isOverJustTime : CurrentMusicTime -> JustTime -> Bool
isOverJustTime currentMusicTime justTime =
    justTime < currentMusicTime


type alias JudgeEffect =
    { keyStr : KeyStr
    , judge : Judge
    , isLongNote : Bool
    }


judgeEffectCmd : JudgeEffect -> Cmd msg
judgeEffectCmd judgeEffect =
    Cmd.batch
        [ AnimationManager.playJudgeEffectAnim
            { keyStr = judgeEffect.keyStr
            , isLongNote = judgeEffect.isLongNote
            }
            |> cmdIf (judgeEffect.judge == Perfect || judgeEffect.judge == Great || judgeEffect.judge == Good)
        , AnimationManager.playJudgeEffectTextAnim
            { keyStr = judgeEffect.keyStr
            , judgeText = toString judgeEffect.judge
            }
        ]
        |> cmdIf (judgeEffect.judge /= Invalid)


playMissEffectAnimCmd : List Judge -> Cmd msg
playMissEffectAnimCmd headNoteJudges =
    AnimationManager.playMissEffectAnim ()
        |> cmdIf (List.member Miss headNoteJudges)
