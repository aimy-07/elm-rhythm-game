module AudioManager.BGM exposing
    ( BGM(..)
    , bgmKeyList
    , fromMusicId
    , sampleFromMusicId
    , unwrap
    )

import AllMusicData.MusicData.MusicId exposing (MusicId)


type BGM
    = TheRoadToHeaven
    | SampleSound
    | SampleSoundShort
    | WhiteGlow
    | SampleSound_Sample
    | SampleSoundShort_Sample
    | WhiteGlow_Sample
    | Invalid


fromMusicId : MusicId -> BGM
fromMusicId musicId =
    case musicId of
        "sampleSound" ->
            SampleSound

        "sampleSoundShort" ->
            SampleSoundShort

        "whiteGlow" ->
            WhiteGlow

        _ ->
            Invalid


sampleFromMusicId : MusicId -> BGM
sampleFromMusicId musicId =
    case musicId of
        "sampleSound" ->
            SampleSound_Sample

        "sampleSoundShort" ->
            SampleSoundShort_Sample

        "whiteGlow" ->
            WhiteGlow_Sample

        _ ->
            Invalid


unwrap : BGM -> Maybe String
unwrap bgm =
    case bgm of
        TheRoadToHeaven ->
            Just "theRoadToHeaven"

        SampleSound ->
            Just "sampleSound"

        SampleSoundShort ->
            Just "sampleSoundShort"

        WhiteGlow ->
            Just "whiteGlow"

        SampleSound_Sample ->
            Just "sampleSound_sample"

        SampleSoundShort_Sample ->
            Just "sampleSoundShort_sample"

        WhiteGlow_Sample ->
            Just "whiteGlow_sample"

        Invalid ->
            Nothing


bgmKeyList : List String
bgmKeyList =
    [ TheRoadToHeaven
    , SampleSound
    , SampleSoundShort
    , WhiteGlow
    , SampleSound_Sample
    , SampleSoundShort_Sample
    , WhiteGlow_Sample
    ]
        |> List.map unwrap
        |> List.filterMap identity
