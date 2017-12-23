port module Database exposing (..)

port setImageDone : String -> Cmd msg

port getDoneImages : String -> Cmd msg

port doneImages : (List String -> msg) -> Sub msg
