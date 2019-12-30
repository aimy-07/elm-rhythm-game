module Page.Play.AudioLoadingS exposing (AudioLoadingS, init, isLoaded, loaded, toAudioUrl)

import AudioManager.AudioUrl exposing (AudioUrl)


type AudioLoadingS
    = NotLoaded
    | Loaded AudioUrl


init : AudioLoadingS
init =
    NotLoaded


loaded : AudioUrl -> AudioLoadingS
loaded audioUrl =
    Loaded audioUrl


isLoaded : AudioLoadingS -> Bool
isLoaded audioLoadingS =
    case audioLoadingS of
        Loaded _ ->
            True

        NotLoaded ->
            False


toAudioUrl : AudioLoadingS -> Maybe AudioUrl
toAudioUrl audioLoadingS =
    case audioLoadingS of
        Loaded audioUrl ->
            Just audioUrl

        NotLoaded ->
            Nothing
