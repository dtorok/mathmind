port module Database exposing (dbSetImageDone, dbGetDoneImages, dbDoneImages, dbExerciseSolved)

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

dbSumScore : (Int -> msg) -> Sub msg
dbSumScore f =
  let
    mapper : List String -> Int
    mapper xs =
      case List.head xs of
        Nothing -> 0
        Just val -> Result.withDefault 0 (String.toInt val)

    f_ : List String -> msg
    f_ xs = f (mapper xs)
  in
    sumScore f_

dbExerciseSolved : String -> Cmd msg
dbExerciseSolved = exerciseSolved

-----
-- PORTED FUNCTIONS
port setImageDone : String -> Cmd msg
port getDoneImages : String -> Cmd msg
port doneImages : (List String -> msg) -> Sub msg
port sumScore : (List String -> msg) -> Sub msg
port exerciseSolved : String -> Cmd msg