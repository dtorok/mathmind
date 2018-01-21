port module Database exposing (dbSetImageDone, dbGetDoneImages, dbUpdateDoneImages, dbAddScore, dbGetScores, dbUpdateScores)

delimiter : String
delimiter = "/:/"

-----
-- API

-- images done
dbSetImageDone : String -> Int -> Cmd msg
dbSetImageDone imageId score =
  let msg = imageId ++ delimiter ++ (toString score)
  in setImageDone msg

dbGetDoneImages : Cmd msg
dbGetDoneImages =
  let msg = ""
  in getDoneImages msg

dbUpdateDoneImages : (List String -> msg) -> Sub msg
dbUpdateDoneImages = updateDoneImages

-- scores
dbAddScore : String -> Cmd msg
dbAddScore = addScore

dbGetScores : Cmd msg
dbGetScores = getScores ""

dbUpdateScores : (List (String, Int) -> msg) -> Sub msg
dbUpdateScores f =
  let
    parser : String -> (String, Int)
    parser s =
      case String.split delimiter s of
        t :: score :: xs ->
          ( t, Result.withDefault 0 (String.toInt score) )
        _ -> ("", 0)

    mapper : List String -> List (String, Int)
    mapper xs = List.map parser xs

    f_ : List String -> msg
    f_ xs = f (mapper xs)
  in
    updateScores f_

-----
-- PORTED FUNCTIONS
port setImageDone : String -> Cmd msg
port getDoneImages : String -> Cmd msg
port updateDoneImages : (List String -> msg) -> Sub msg

port addScore : String -> Cmd msg
port getScores : String -> Cmd msg
port updateScores : (List String -> msg) -> Sub msg
