port module Ports exposing (backspaceKeyPressed, saveHistory, searchKeyPressed, updateHistory)


port saveHistory : List String -> Cmd msg


port updateHistory : (List String -> msg) -> Sub msg


port searchKeyPressed : (String -> msg) -> Sub msg


port backspaceKeyPressed : (Bool -> msg) -> Sub msg
