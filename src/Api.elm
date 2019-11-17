module Api exposing (Data, get_data)

-- import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Http as Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Session exposing (Session)
-- import Url.Builder exposing (QueryParameter)



-- apiUrl =
--     "/api"
-- Coresponds to API exposed at : https://jsonplaceholder.typicode.com


type Endpoint
    = GetData


-- toUrl endpoint =
--     case endpoint of
--         GetData ->
--             url [ "albums" ] []



-- TODO What about internal endpoints ?


-- url : List String -> List QueryParameter -> String
-- url paths queryParams =
--     Url.Builder.crossOrigin "https://jsonplaceholder.typicode.com" paths queryParams


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
        , expect = Http.expectJson (\ x -> handler (RemoteData.fromResult x)) decoder}
