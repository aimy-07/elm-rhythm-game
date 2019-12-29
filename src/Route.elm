module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import MusicInfo.CsvFileName exposing (CsvFileName)
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query



-- ROUTING


type Route
    = Title
    | Home
    | Play (Maybe CsvFileName)
    | Error


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Title Parser.top
        , Parser.map Home (s "home")
        , Parser.map Play (s "play" <?> Query.string "q")
        , Parser.map Error (s "error")
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
    case page of
        Title ->
            "/"

        Home ->
            "/home"

        Play maybeCsvFileName ->
            maybeCsvFileName
                |> Maybe.map (\csvFileName -> "/play?q=" ++ csvFileName)
                |> Maybe.withDefault "/play"

        Error ->
            "/error"
