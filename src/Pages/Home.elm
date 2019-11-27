module Pages.Home exposing (Model, Msg, init, update, view)

import Api exposing (Data, get_data)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles



-- Model
-- { session : Session, contacts : List Contact }

type alias Model =
    { data : WebData (List Data) }



-- State


type Msg
    = NoOp
    | HandleData (WebData (List Data))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { data = NotAsked }, load_data session )


load_data session =
    get_data HandleData



-- Api.sendQuery session GotContacts getContacts


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleData res ->
            ( { model | data = res }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View

-- TODO add onclick go to details page with this data

display_data data =
    Element.row [] [text <| "Title : " ++ data.title]

render model =
    case model.data of
        Loading ->
            text "Loading cat data, please stand by..."

        Success data ->
            let
                _ = Debug.log "data" data
            in
                Element.column [] <| List.map display_data data

        Failure error ->
            let
                _ = Debug.log "error" error
            in
            text ("Oh noes, cat loading failed with error: ")

        NotAsked ->
            text "Not yet attempted load"


view : Model -> TitleAndContent Msg
view model =
    { title = "Contacts"
    , content = render model
    }
