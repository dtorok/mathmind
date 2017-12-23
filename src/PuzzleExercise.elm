module PuzzleExercise exposing (..)

import Html exposing (Html, div, img, table, tr, td, text)
import Html.Attributes exposing (class, src, width, height)
import Html.Events exposing (onClick)
import Exercise
import Task
import Dom

-----
-- MODELS
type alias Coord = (Int, Int)
type alias Board = List (List Cell)
type Cell = Visible | Hidden
type alias Image =
  { imageId: String
  , url: String
  , width: Int
  , height: Int
  , rows: Int
  , cols: Int
  }

type alias Puzzle =
  { image: Image
  , chosen: Maybe Coord
  , exModel: Maybe Exercise.Model
  , board: Board
  , uncoveredFields: Int
  , isDone: Bool
  }

initPuzzle : Image -> Bool -> Puzzle
initPuzzle image isDone =
  { image = image
  , chosen = Nothing
  , exModel = Nothing
  , board = List.repeat image.rows <| List.repeat image.cols Hidden
  , uncoveredFields = image.rows * image.cols
  , isDone = isDone}

makeVisible : Board -> Maybe Coord -> Board
makeVisible board maybeCoord =
  case maybeCoord of
    Nothing -> board
    Just (r, c) ->
      (flip List.indexedMap) board
        (\br row ->
          (flip List.indexedMap) row
            (\bc cell ->
              if (r == br && c == bc) then
                Visible
              else
                cell
            )
        )

initImage : String -> String -> Int -> Int -> Int -> Int -> Image
initImage imageId url width height rows cols =
  let
    localUrl = "resources/images/" ++ imageId
  in
    { imageId = imageId
    , url = localUrl
    , width = width
    , height = height
    , rows = rows
    , cols = cols
    }

isDone : Puzzle -> Bool
isDone model = model.uncoveredFields == 0


-----
-- MESSAGES
type Msg
  = NoOp
  | ChooseCell Coord
  | ExMsg Exercise.Msg


-----
-- VIEW
view : Puzzle -> Html Msg
view = viewImageAndTable

viewImageAndTable : Puzzle -> Html Msg
viewImageAndTable model =
  div [ class "image" ]
    [ viewImage model
    , viewTableIfNotDone model
    ]

viewImage : Puzzle -> Html Msg
viewImage model =
  img
    [ class "image"
    , src model.image.url
    , width model.image.width
    , height model.image.height ] []

viewTableIfNotDone : Puzzle -> Html Msg
viewTableIfNotDone model =
  if model.isDone then
    div [] []
  else
    viewTable model

viewTable : Puzzle -> Html Msg
viewTable model =
  let
    getClass cell = case cell of
      Visible -> "visible"
      Hidden -> "hidden"

    getChosenClass chosen r c = case chosen of
      Nothing -> ""
      Just (cr, cc) -> if (r == cr && c == cc) then "chosen" else ""

  in
    table [width model.image.width, height model.image.height] <|
      (flip List.indexedMap) model.board
        (\r row ->
          tr [] <|
            (flip List.indexedMap) row
              (\c cell ->
                td [class (getClass cell), class (getChosenClass model.chosen r c), onClick (ChooseCell (r, c))] []
              )
        )

viewExercise : Puzzle -> Html Msg
viewExercise model =
  case model.exModel of
    Nothing -> text ""
    Just exModel ->
      let exHtml = Exercise.view exModel
      in Html.map ExMsg exHtml

-----
-- UPDATE
update : Msg -> Puzzle -> (Puzzle, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    ChooseCell coord ->
      let
        domId = "exercise"
        (exModel, exCmd) = Exercise.init domId
        model_ = { model
                  | chosen = Just coord
                  , exModel = Just exModel
                  }
        cmdFocus = Task.attempt (always NoOp) (Dom.focus domId)
        cmdExCmd = Cmd.map ExMsg exCmd
      in
        (model_, Cmd.batch [cmdExCmd, cmdFocus])
    ExMsg exMsg ->
      case model.exModel of
        Nothing -> (model, Cmd.none)
        Just exModel ->
          let
            (exModel_, exCmd) = Exercise.update exMsg exModel
            model_ = { model | exModel = Just exModel_ }
          in
            if Exercise.isCorrect exModel_ then
              (model, Cmd.none)
              |> andThen updateExerciseCorrect
              |> andThen updateUncoveredFields
            else
              ( model_, Cmd.map ExMsg exCmd )

updateExerciseCorrect : Puzzle -> (Puzzle, Cmd Msg)
updateExerciseCorrect model =
  let
    model_ = { model
            | exModel = Nothing
            , chosen = Nothing
            , board = makeVisible model.board model.chosen }
  in
    (model_, Cmd.none)

updateUncoveredFields : Puzzle -> (Puzzle, Cmd Msg)
updateUncoveredFields model =
  let
    model_ = { model
             | uncoveredFields = model.uncoveredFields - 1 }
  in
    (model_, Cmd.none)

-----
-- HELPERS
andThen : (model -> (model, Cmd a)) -> (model, Cmd a) -> (model, Cmd a)
andThen f (model, cmd) =
  let (model_, cmd_) = f model
  in (model_, Cmd.batch [cmd, cmd_])
