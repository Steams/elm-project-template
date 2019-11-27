module Route exposing (Route(..), fromUrl, replaceUrl, toUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s, string, top)


type Route
    = Root
    | Details String
    | Home
    | Login


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home top
        , Parser.map Details (s "details" </> string)
        , Parser.map Login (s "login")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toUrl : Route -> String
toUrl route =
    let
        pathSegments =
            case route of
                Root ->
                    []

                Home ->
                    []

                Login ->
                    [ "login" ]

                Details id ->
                    [ "details", id ]
    in
    "/" ++ String.join "/" pathSegments


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toUrl route)
