port module Ports exposing (..)

import Json.Encode as E

port storeSession : E.Value -> Cmd msg
