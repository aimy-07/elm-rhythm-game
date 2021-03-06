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
import Constants exposing (goodRange, niceRange, perfectRange)
import Page.Play.CurrentMusicTime exposing (CurrentMusicTime)
import Page.Play.Key as Key exposing (Key)
import Page.Play.Note.JustTime exposing (JustTime)
import Utils exposing (cmdIf)


type Judge
    = Perfect
    | Nice
    | Good
    | Lost
    | Miss
    | Invalid


toString : Judge -> String
toString judge =
    case judge of
        Perfect ->
            "Just"

        -- Justは予約後のため、プログラム内ではPerfectとして扱っている
        Nice ->
            "Nice"

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

    else if Basics.abs (justTime - currentMusicTime) < niceRange then
        Nice

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
    { key : Key
    , judge : Judge
    , isLongNote : Bool
    }


judgeEffectCmd : JudgeEffect -> Cmd msg
judgeEffectCmd judgeEffect =
    Cmd.batch
        [ AnimationManager.playJudgeEffectAnim
            { key = Key.unwrap judgeEffect.key
            , isLongNote = judgeEffect.isLongNote
            }
            |> cmdIf (judgeEffect.judge == Perfect || judgeEffect.judge == Nice || judgeEffect.judge == Good)
        , AnimationManager.playJudgeEffectTextAnim
            { key = Key.unwrap judgeEffect.key
            , judgeText = toString judgeEffect.judge
            }
        ]
        |> cmdIf (judgeEffect.judge /= Invalid)


playMissEffectAnimCmd : List Judge -> Cmd msg
playMissEffectAnimCmd headNoteJudges =
    AnimationManager.playMissEffectAnim ()
        |> cmdIf (List.member Miss headNoteJudges)
