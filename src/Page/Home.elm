port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import MusicInfo.Mode as Mode exposing (Mode)
import Page.Home.AllMusicInfoList as AllMusicInfoList exposing (AllMusicInfoList)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , allMusicInfoList : AllMusicInfoList
    , selectingMode : Mode
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , allMusicInfoList = AllMusicInfoList.init
      , selectingMode = Mode.normal
      }
    , getAllMusicInfoList ()
    )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)
    | ChangeMode Mode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllMusicInfoList musicInfoDtos ->
            let
                nextAllMusicInfoList =
                    musicInfoDtos
                        |> List.map MusicInfo.new
                        |> AllMusicInfoList.create
            in
            ( { model | allMusicInfoList = nextAllMusicInfoList }, Cmd.none )

        ChangeMode mode ->
            ( { model | selectingMode = mode }, Cmd.none )



-- PORT


port getAllMusicInfoList : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        filterMusicInfoList =
            model.allMusicInfoList
                |> AllMusicInfoList.filterByMode model.selectingMode
    in
    { title = "Home"
    , content =
        div []
            [ h1 [] [ text "ホーム画面" ]
            , div []
                [ viewModeTabBtn Mode.normal
                , viewModeTabBtn Mode.hard
                , viewModeTabBtn Mode.master
                ]
            , AllMusicInfoList.view filterMusicInfoList
            ]
    }


viewModeTabBtn : Mode -> Html Msg
viewModeTabBtn mode =
    button [ onClick <| ChangeMode mode ] [ text <| Mode.unwrap mode ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotAllMusicInfoList GotAllMusicInfoList
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
