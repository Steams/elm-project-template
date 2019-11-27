module Pages.Login exposing (Model, Msg(..), init, update, view)

import Api exposing (login)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as HtmlAttribute
import Json.Encode as E
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles



-- Model


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = NoOp
    | UsernameInput String
    | PasswordInput String
    | SubmitLogin
    | LoginResponse (WebData String)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { username = "", password = "" }, Cmd.none )


login username password =
    Api.login LoginResponse username password


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        UsernameInput val ->
            ( { model | username = val }
            , Cmd.none
            )

        PasswordInput val ->
            ( { model | password = val }
            , Cmd.none
            )

        SubmitLogin ->
            ( { model | password = "", username = "" }
            , login model.username model.password
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View


button value handler =
    Input.button
        [ height (px 60)
        , width (px 300)
        , centerX
        , Border.rounded 100
        , Border.width 2
        , Border.color Styles.blue
        , Font.size 12
        , Font.center
        , Font.color Styles.blue
        , Font.bold
        ]
        { onPress = Just handler
        , label = text value
        }


input placeholder handler value =
    Input.text
        [ width (px 400)
        , height (px 65)
        , centerX
        , paddingXY 20 22
        , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = Styles.black } ]
        , Border.width 0
        , Border.rounded 0
        , Font.size 20
        , Background.color Styles.input_background
        ]
        { onChange = handler
        , text = value
        , placeholder = Just (Input.placeholder [] <| el [ centerY, Font.color (rgb255 200 200 200) ] <| text placeholder)
        , label = Input.labelHidden ""
        }


title value =
    el
        [ centerX
        , Font.size 35
        , Font.family [ Font.typeface "Roboto" ]
        , Font.bold
        , Font.color Styles.primary_green
        ]
    <|
        text value


login_panel username password =
    Element.column [ width fill, spacing 60, centerY ]
        [ title "Log In"
        , Element.column [ centerX, spacing 20 ]
            [ input "Username" UsernameInput username
            , input "Password" PasswordInput password
            ]
        , el [ centerX ] <| text "Forgot your password ?"
        , button "LOG IN" SubmitLogin
        ]


render model =
    Element.row [ width fill, height fill ]
        [ login_panel model.username model.password]


view : Model -> TitleAndContent Msg
view model =
    { title = "Login"
    , content = render model
    }
