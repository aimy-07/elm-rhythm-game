module Page.Home.MusicInfo exposing
    ( MusicInfo
    , new
    , normalMusics
    , toBpm
    , toComposer
    , toFullTime
    , toLevel
    , toMusicName
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)


type MusicInfo
    = MusicInfo
        { musicName : String
        , composer : String
        , level : Int
        , fullTime : Float
        , bpm : Int
        }


new :
    { musicName : String
    , composer : String
    , level : Int
    , fullTime : Float
    , bpm : Int
    }
    -> MusicInfo
new rawMusicInfo =
    MusicInfo
        { musicName = rawMusicInfo.musicName
        , composer = rawMusicInfo.composer
        , level = rawMusicInfo.level
        , fullTime = rawMusicInfo.fullTime
        , bpm = rawMusicInfo.bpm
        }


toMusicName : MusicInfo -> String
toMusicName (MusicInfo { musicName }) =
    musicName


toComposer : MusicInfo -> String
toComposer (MusicInfo { composer }) =
    composer


toLevel : MusicInfo -> Int
toLevel (MusicInfo { level }) =
    level


toFullTime : MusicInfo -> Float
toFullTime (MusicInfo { fullTime }) =
    fullTime


toBpm : MusicInfo -> Int
toBpm (MusicInfo { bpm }) =
    bpm


view : MusicInfo -> Html msg
view (MusicInfo { musicName, composer, level, fullTime, bpm }) =
    div []
        [ div [] [ text musicName ]
        , div [] [ text composer ]
        , div []
            [ text <| "Lv. " ++ String.fromInt level
            , text <| "\u{3000}Time: " ++ toStringTime fullTime
            , text <| "\u{3000}Bpm: " ++ String.fromInt bpm
            ]
        ]


toStringTime : Float -> String
toStringTime time =
    let
        min =
            modBy 60 (Basics.round (time / 1000))

        sec =
            Basics.round (time / 1000) // 60
    in
    String.fromInt min ++ "分" ++ String.fromInt sec ++ "秒"


normalMusics : List MusicInfo
normalMusics =
    [ new
        { musicName = "sample_sound"
        , composer = "作曲：xxxx"
        , level = 1
        , fullTime = 28
        , bpm = 108
        }
    , new
        { musicName = "sample_sound"
        , composer = "作曲：xxxx"
        , level = 1
        , fullTime = 28
        , bpm = 108
        }
    , new
        { musicName = "sample_sound"
        , composer = "作曲：xxxx"
        , level = 1
        , fullTime = 28
        , bpm = 108
        }
    ]
