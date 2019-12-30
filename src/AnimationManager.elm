port module AnimationManager exposing
    ( playComboEffectAnim
    , playJudgeEffectAnim
    , playJudgeEffectTextAnim
    , playMusicSelectAnim
    )


port playMusicSelectAnim : () -> Cmd msg


port playComboEffectAnim : () -> Cmd msg


port playJudgeEffectAnim : { keyStr : String, isLongNote : Bool } -> Cmd msg


port playJudgeEffectTextAnim : { keyStr : String, judgeText : String } -> Cmd msg
