module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import MusicInfo.CsvFileName exposing (CsvFileName)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = Home
    | Login
    | Play CsvFileName


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Play (s "play" </> string)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Play csvFileName ->
                    [ "play", csvFileName ]
    in
    "/" ++ String.join "/" pieces
