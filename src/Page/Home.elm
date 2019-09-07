port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MusicInfo as MusicInfo exposing (MusicInfo, MusicInfoDto)
import Page.Home.AllMusicInfoList as AllMusicInfoList exposing (AllMusicInfoList)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , allMusicInfoList : AllMusicInfoList
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , allMusicInfoList = AllMusicInfoList.init
      }
    , getAllMusicInfoList ()
    )



-- UPDATE


type Msg
    = GotAllMusicInfoList (List MusicInfoDto)


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



-- PORT


port getAllMusicInfoList : () -> Cmd msg


port gotAllMusicInfoList : (List MusicInfoDto -> msg) -> Sub msg



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div []
            [ h1 [] [ text "ホーム画面" ]
            , AllMusicInfoList.view model.allMusicInfoList
            ]
    }



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
