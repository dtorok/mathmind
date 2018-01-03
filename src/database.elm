port module Database exposing (dbSetImageDone, dbGetDoneImages, dbDoneImages)

-----
-- API
dbSetImageDone : String -> Int -> Cmd msg
dbSetImageDone imageId score =
  let msg = imageId ++ "/:/" ++ (toString score)
  in setImageDone msg

dbGetDoneImages : Cmd msg
dbGetDoneImages =
  let msg = ""
  in getDoneImages msg

dbDoneImages : (List String -> msg) -> Sub msg
dbDoneImages = doneImages

-----
-- PORTED FUNCTIONS
port setImageDone : String -> Cmd msg
port getDoneImages : String -> Cmd msg
port doneImages : (List String -> msg) -> Sub msg
