port module Ports exposing (..)

import Json.Encode as Encode


port saveHistory : List String -> Cmd msg

port updateHistory : (List String -> msg) -> Sub msg

port searchKeyPressed : (String -> msg) -> Sub msg
