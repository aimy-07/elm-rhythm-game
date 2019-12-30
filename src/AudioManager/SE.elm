module AudioManager.SE exposing (SE(..), toAudioUrl)

import AudioManager.AudioUrl exposing (AudioUrl)


type SE
    = MusicSelect


toAudioUrl : SE -> AudioUrl
toAudioUrl se =
    case se of
        MusicSelect ->
            "./audios/se_music_select.mp3"
