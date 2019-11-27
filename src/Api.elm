module Api exposing (Data, get_data, login)

import Http as Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Session exposing (Session)


type Endpoint
    = GetData


type alias Data =
    { userId : Int
    , id : Int
    , title : String
    }


data_decoder : Decoder Data
data_decoder =
    Decode.map3 Data
        (field "userId" int)
        (field "id" int)
        (field "title" string)


data_list_decoder : Decode.Decoder (List Data)
data_list_decoder =
    Decode.list data_decoder


get_data : (WebData (List Data) -> msg) -> Cmd msg
get_data handler =
    get "https://jsonplaceholder.typicode.com/albums" handler data_list_decoder



-- get : Endpoint -> (WebData a -> msg) -> Decoder a -> Cmd msg


get : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
get endpoint handler decoder =
    Http.get
        { url = endpoint
        , expect = Http.expectJson (\x -> handler (RemoteData.fromResult x)) decoder
        }


login : (WebData String -> msg) -> String -> String -> Cmd msg
login handler username password =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/login"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string username )
                    , ( "password", Encode.string password )
                    ]
        , expect = Http.expectJson (RemoteData.fromResult >> handler) string
        , timeout = Nothing
        , tracker = Nothing
        }
