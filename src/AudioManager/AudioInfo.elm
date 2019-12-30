module AudioManager.AudioInfo exposing (AudioInfo, AudioInfoDto, new)

import AudioManager.AudioUrl exposing (AudioUrl)


type alias AudioInfo =
    { audioFileName : String
    , audioUrl : AudioUrl
    }


type alias AudioInfoDto =
    { audioFileName : String
    , audioUrl : String
    }


new : AudioInfoDto -> AudioInfo
new { audioFileName, audioUrl } =
    { audioFileName = audioFileName
    , audioUrl = audioUrl
    }
