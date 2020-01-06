port module AnimationManager exposing
    ( playComboEffectAnim
    , playJudgeEffectAnim
    , playJudgeEffectTextAnim
    , playMissEffectAnim
    , playMusicSelectAnim
    )


port playMusicSelectAnim : () -> Cmd msg


port playJudgeEffectAnim : { key : String, isLongNote : Bool } -> Cmd msg


port playJudgeEffectTextAnim : { key : String, judgeText : String } -> Cmd msg


port playMissEffectAnim : () -> Cmd msg


port playComboEffectAnim : () -> Cmd msg
